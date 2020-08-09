#include "pch.h"
#include "launcher.h"
#include "platform/simpleApp.h"
#include "gpu/public.h"
#include "input/inputDevice.h"
#include "input/inputCollector.h"
#include "renderer/public.h"
#include "audio/public.h"
#include "import/public.h"		// todo: move that to toolset, then load through resource system

#include "gameActions.h"

#include "cameraSystem.h"
#include "playerMovementSystem.h"
#include "transformSystem.h"
#include "projectileSystem.h"
#include "debugMeshRenderSystem.h"
#include "spriteSystem.h"
#include "tiledTerrainSystem.h"

#include "crend/public.h"
#include "cphysics/public.h"

using namespace game;
using namespace math;

constexpr uint32 c_width = 1600;
constexpr uint32 c_height = 900;

const char* c_exampleModelPath = "../../assets/grass_tile_01.obj";
const char* c_albedoTexturePath = "../../assets/chest/chest_albedo.png";
const char* c_ambientOcclusionTexturePath = "../../assets/chest/chest_ao.png";
const char* c_fontPath = "../../assets/fonts/legendaria-64";	// without extension, see Font class for more info
const char* c_spriteLibraryPath = "../../assets/sprites/bullets";	// without extension, see SpriteLibrary class for more info
const char* c_shadersDirectoryPath = "../../source/game/duena/internal/";

const char* c_meshesPaths[] = {"../../assets/box_rigged/box_rigged.fbx", "../../assets/chest/chest.obj"};
const char* c_rigsPaths[] = {"../../assets/box_rigged/box_rigged.fbx"};
const char* c_animationPaths[] = {"../../assets/box_rigged/box_rigged.fbx"};

// note: paths for mac when using fopen requires additional "../../../" because of bundle
const char* c_basicVertexShaderPath = "../../../../../shaders/vert.spv";
const char* c_basicFragmentShaderPath = "../../../../../shaders/frag.spv";

void MainGameLoopJob(jobs::Context& ctx);

namespace anim
{
	struct Rig
	{
		DynArray<StringHash> m_jointNames;
		DynArray<int16> m_parents;
		DynArray<math::Transform> m_referencePose;
	};
	
	struct JointDataOffset
	{
		uint32 m_translationOffset;
		uint32 m_translationLength;
		
		uint32 m_rotationOffset;
		uint32 m_rotationLength;
	};
	
	struct Animation
	{
		uint32 m_duration;
		
		DynArray<JointDataOffset> m_offsets;
		
		DynArray<uint32> m_translationTimes;
		DynArray<math::Vector3> m_translations;
		
		DynArray<uint32> m_rotationTimes;
		DynArray<math::Quaternion> m_rotations;
	};
	
	struct Pose
	{
		uint32 m_numTransforms;
		math::Transform* m_transforms;
	};
	
	uint32 LowerBoundEqual(const uint32* arr, uint32 size, uint32 element)
	{
		uint32 i, step;
		uint32 first = 0;
		
		while(0 < size)
		{
			i = first;
			step = size / 2;
			i += step;
			
			if(arr[i] <= element)
			{
				first = ++i;
				size -= step + 1;
			}
			else
			{
				size = step;
			}
		}
		
		return first;
	}
	
	void Animation_SampleLinear(const Animation& animation, Pose& pose, float time)
	{
		constexpr float fps = 30.0f;
		constexpr float frequency = 1.0f / fps;
		
		const uint32 duration = animation.m_duration;
		
		FUR_ASSERT(duration > 0);
		
		time = math::Clamp(time, 0.0f, duration * frequency);
		
		const float frame = time * fps;
		const uint32 lowerFrame = (uint32)(frame);
		const float fraction = math::Clamp(frame - (float)lowerFrame, 0.0f, 1.0f);
		
		const uint32 numTransforms = pose.m_numTransforms;
		
		for(uint32 j=0; j<numTransforms; ++j)
		{
			const JointDataOffset offset = animation.m_offsets[j];
			
			math::Vector3 t_c;
			math::Quaternion r_c;
			
			// translation
			{
				const uint32* times = &animation.m_translationTimes[offset.m_translationOffset];
				
				const uint32 i = LowerBoundEqual(times, offset.m_translationLength, lowerFrame);
				FUR_ASSERT(i < offset.m_translationLength);
				
				if(0.0f < fraction && (i < offset.m_translationLength - 1))
				{
					math::Vector3 a = animation.m_translations[i];
					math::Vector3 b = animation.m_translations[i+1];
				
					const uint32 i_u = animation.m_translationTimes[i+1];
					const uint32 distance = i_u - i;
					const uint32 progressDistance = lowerFrame - i;
					
					const float f_distance = distance * frequency;
					const float f_progressDistance = (progressDistance + fraction) * frequency;
					
					const float alpha = f_progressDistance / f_distance;
					
					t_c = math::Lerp(a, b, alpha);
				}
				else
				{
					t_c = animation.m_translations[i];	// this case is actually super rare, usually we hit between frames
				}
			}
			
			// rotation
			{
				const uint32* times = &animation.m_rotationTimes[offset.m_rotationOffset];
				
				const uint32 i = LowerBoundEqual(times, offset.m_rotationLength, lowerFrame);
				FUR_ASSERT(i < offset.m_rotationLength);
				
				if(0.0f < fraction && i < (offset.m_rotationLength - 1))
				{
					math::Quaternion a = animation.m_rotations[i];
					math::Quaternion b = animation.m_rotations[i+1];
					
					const uint32 i_u = animation.m_rotationTimes[i+1];
					const uint32 distance = i_u - i;
					const uint32 progressDistance = lowerFrame - i;
					
					const float f_distance = distance * frequency;
					const float f_progressDistance = (progressDistance + fraction) * frequency;
					
					const float alpha = f_progressDistance / f_distance;
					
					r_c = math::Lerp(a, b, alpha);	// for sampling we do cheap lerp + normalize instead of heavy slerp
					math::NormalizeRef(r_c);
				}
				else
				{
					r_c = animation.m_rotations[i];	// this case is actually super rare, usually we hit between frames
				}
			}
			
			math::Transform& transform = pose.m_transforms[j];
			
			transform.translation.x = t_c.x;
			transform.translation.y = t_c.y;
			transform.translation.z = t_c.z;
			transform.translation.w = 0.0f;
			
			transform.rotation = r_c;
		}
	}
}

struct ResourceMap
{
	HashMap<ResourceID, UniquePtr<rend::Mesh>> m_meshes;
	HashMap<ResourceID, UniquePtr<gpu::Texture>> m_textures;
	HashMap<ResourceID, UniquePtr<anim::Rig>> m_rigs;
	HashMap<ResourceID, UniquePtr<anim::Animation>> m_animations;
};

// Game engine
struct GameEngine
{
	rend::Renderer m_renderer;
	input::InputCollector m_inputCollector;
	UniquePtr<gpu::Buffer> m_testInstanceMatrices;
	UniquePtr<gpu::Buffer> m_testInstanceColors;
	UniquePtr<gpu::Texture> m_albedoTexture;
	UniquePtr<gpu::Texture> m_ambientOcclusionTexture;
	
	DynArray<game::IGameSystem*> m_gameSystemsByUpdateOrder;
	DynArray<game::IGameSystem*> m_gameSystemsByID;
	
	UniquePtr<gpu::Buffer> m_testSpriteInstances;
	
	EntityIDGenerator m_endityIDGenerator;
	
	ResourceMap m_resources;
	
	template<typename T>
	void PushBackGameSystem(T* system)
	{
		m_gameSystemsByUpdateOrder.push_back(system);
		uint32 id = GameSystemID<T>();
		if(m_gameSystemsByID.size() <= id)
			m_gameSystemsByID.resize(id+1);
		
		m_gameSystemsByID[id] = system;
	}
	
	audio::AudioEngine m_audioEngine;
};

class SimpleApp : public platform::ISimpleApp
{
public:
    virtual bool Initialize()
    {
		//jobs::InitializeJobSystem();
		m_engine.m_renderer.Initialize({c_width, c_height, c_shadersDirectoryPath, c_fontPath, c_spriteLibraryPath});
		
		if(!m_engine.m_audioEngine.Initialize())
			return false;
		
		// start main game loop job
		{
			jobs::JobDesc desc;
			desc.m_jobFunc = MainGameLoopJob;
			desc.m_userData = &m_engine;
			//jobs::RunJobs(&desc, 1);
		}
		
		bool result = LoadResources_TEMP();
		FUR_ASSERT(result);
		
		result = LoadTextures_TEMP();
		FUR_ASSERT(result);
		
		// initialize game systems
		{
			// when you're adding new system, add it here
			// the order of push backs creates the order of update
			// todo: add tick groups
			m_engine.PushBackGameSystem(new PlayerMovementSystem());
			m_engine.PushBackGameSystem(new CameraSystem());
			m_engine.PushBackGameSystem(new TransformSystem());
			m_engine.PushBackGameSystem(new ProjectileSystem());
			m_engine.PushBackGameSystem(new DebugMeshRenderSystem());
			m_engine.PushBackGameSystem(new SpriteSystem());
			m_engine.PushBackGameSystem(new TiledTerrainSystem());
			
			// initialize every system
			DynArray<game::IGameSystem*>& systems = m_engine.m_gameSystemsByUpdateOrder;
			game::InitializeSystemContext ctx;
			
			for(uint32 i=0; i<systems.size(); ++i)
			{
				systems[i]->Initialize(ctx);
			}
		}
		
		GameSystemsProvider gameSystemsProvider;
		gameSystemsProvider.m_systems = {m_engine.m_gameSystemsByID.data(), m_engine.m_gameSystemsByID.size()};
		
		TransformSystem* transformSystem = gameSystemsProvider.GetSystem<TransformSystem>();
		PlayerMovementSystem* playerMovementSystem = gameSystemsProvider.GetSystem<PlayerMovementSystem>();
		CameraSystem* cameraSystem = gameSystemsProvider.GetSystem<CameraSystem>();
		DebugMeshRenderSystem* debugMeshRenderSystem = gameSystemsProvider.GetSystem<DebugMeshRenderSystem>();
		SpriteSystem* spriteSystem = gameSystemsProvider.GetSystem<SpriteSystem>();
		TiledTerrainSystem* terrainSystem = gameSystemsProvider.GetSystem<TiledTerrainSystem>();
		
		// custom initialization
		{
			spriteSystem->Initialize(m_engine.m_renderer.GetTestSpriteLibrary());
		}
		
		// spawn player 1
		{
			EntityID id = m_engine.m_endityIDGenerator.GetNewID();
			
			math::Transform transform = math::CreateTransformIdentity();
			transform.rotation = math::CreateQuaternionRotationAxis(0.0f, 0.0f, 1.0f, 180_deg);
			transformSystem->CreateTransform(id, transform);
			playerMovementSystem->AddPlayer(id);
			cameraSystem->AddEntityToFollow(id);
			
			const rend::SpriteLibrary* spriteLibrary = m_engine.m_renderer.GetTestSpriteLibrary();
			rend::SpriteID spriteID = spriteLibrary->GetSpriteID("ballflying");
			uint32 fps = spriteLibrary->GetSpriteFPS(spriteID);
			uint32 numFrames = spriteLibrary->NumFramesForSprite(spriteID);
			
			spriteSystem->CreateSprite(id, spriteID, numFrames, fps, {-0.5f, -0.5f}, {0.8f, 0.8f}, {1.0f, 0.5f, 0.0f});
		}
		
		// spawn player 2
#if 0
		{
			EntityID id = m_engine.m_endityIDGenerator.GetNewID();
			
			math::Transform transform = math::CreateTransformIdentity();
			transform.translation.x = 1.0f;
			transform.rotation = math::CreateQuaternionRotationAxis(0.0f, 0.0f, 1.0f, 180_deg);
			transformSystem->CreateTransform(id, transform);
			playerMovementSystem->AddPlayer(id);
			cameraSystem->AddEntityToFollow(id);
			
			const rend::SpriteLibrary* spriteLibrary = m_engine.m_renderer.GetTestSpriteLibrary();
			rend::SpriteID spriteID = spriteLibrary->GetSpriteID("ballflying");
			uint32 fps = spriteLibrary->GetSpriteFPS(spriteID);
			uint32 numFrames = spriteLibrary->NumFramesForSprite(spriteID);
			
			spriteSystem->CreateSprite(id, spriteID, numFrames, fps, {-0.5f, -0.5f}, {0.8f, 0.8f}, {0.0f, 0.0f, 1.0f});
		}
#endif
		
		// procedural world creation
		{
			const int32 start_x = -10;
			const int32 start_y = -10;
			float offset_xy = 0.5f;
			
			double rotations[4] = {0_deg, 90_deg, 180_deg, 270_deg};
			
			for(int32 w=0; w<20; ++w)
			{
				for(int32 h=0; h<20; ++h)
				{
					EntityID id = m_engine.m_endityIDGenerator.GetNewID();
					math::Transform transform = math::CreateTransformIdentity();
					transform.translation = {(float)(start_x + w) + offset_xy, (float)(start_y + h) + offset_xy,
						((float)(std::rand()%10)/10.0f) * 0.05f - 0.1f, 0.0f};
					transform.rotation = math::CreateQuaternionRotationAxis(0.0f, 0.0f, 1.0f, rotations[(std::rand())%4]);
					transformSystem->CreateTransform(id, transform);
					debugMeshRenderSystem->VisualizeEntity(id, 1.0f, {1,1,1});
				}
			}
		}
		
		// tiled terrain creation
		{
			WorldGeneratorContext ctx;
			ctx.m_width = 100;
			ctx.m_height = 100;
			ctx.m_elevationVariety = 10;
			ctx.m_visualVariety = 1;
			
			terrainSystem->GenerateWorld(ctx);
		}
		
		// create sprite instances buffer
		{
			gpu::BufferDesc desc;
			desc.m_data = nullptr;
			desc.m_size = sizeof(game::SpriteInstance) * 2048;
			desc.m_options = gpu::ResourceOptions::CpuCacheModeDefaultCache;
			m_engine.m_testSpriteInstances = m_engine.m_renderer.GetDevice()->CreateBuffer(desc);
		}
		
        return result;
    }
    
    virtual gpu::Device* GetDevice()
    {
        return m_engine.m_renderer.GetDevice();
    }
    
	virtual bool DrawViewport(platform::ViewportInfo& viewport, const TimeInfo& timeInfo)
    {
		float timeDelta = timeInfo.dt;
		
		// todo: this should be async, on fiber
		{
			rend::ConstantBufferData sceneData;
			
			// apply inputs
			DynArray<input::InputEvent> inputEvents;
			m_engine.m_inputCollector.CollectInput(inputEvents);
			
			static float s_actionsPlayer1[GA_ACTIONS_COUNT];
			static float s_actionsPlayer2[GA_ACTIONS_COUNT];
			MapInputToActionValues(ArrayView<const input::InputEvent>(inputEvents.data(), inputEvents.size()), s_actionsPlayer1, s_actionsPlayer2);
			
			DynArray<game::IGameSystem*>& systems = m_engine.m_gameSystemsByID;
			GameSystemsProvider gameSystemsProvider;
			gameSystemsProvider.m_systems = {systems.data(), systems.size()};
			
			// tick core systems
			{
				m_engine.m_audioEngine.Update();
			}
			
			// tick game
			{
				constexpr uint32 SPAWN_COMMAND_BUFFER_DATA_SIZE = 1024;
				uint8 spawnCommandsData[SPAWN_COMMAND_BUFFER_DATA_SIZE];
				fur::CommandBuffer spawnCommands{spawnCommandsData, 0, SPAWN_COMMAND_BUFFER_DATA_SIZE};
				
				constexpr uint32 DESPAWN_CAPACITY = 1024;
				EntityID despawnedEntities[DESPAWN_CAPACITY];
				uint32 numDespawnedEntities = 0;
				
				EntitySpawnQueue spawnQueue{fur::CommandEncoder(&spawnCommands)};
				EntityDespawnQueue despawnQueue(despawnedEntities, &numDespawnedEntities, DESPAWN_CAPACITY);
				
				// tick every system
				{
					game::TickSystemContext ctx;
					ctx.m_actionsPlayer1 = {s_actionsPlayer1, GA_ACTIONS_COUNT};
					ctx.m_actionsPlayer2 = {s_actionsPlayer2, GA_ACTIONS_COUNT};
					ctx.m_systems = gameSystemsProvider;
					ctx.m_spawn = &spawnQueue;
					ctx.m_despawn = &despawnQueue;
					
					float dt = timeDelta;
					
					for(uint32 i=0; i<systems.size(); ++i)
					{
						// todo: introduce tick groups
						systems[i]->Tick(ctx, dt);
					}
				}
				
				// perform despawning
				{
					uint32 numSystems = (uint32)m_engine.m_gameSystemsByUpdateOrder.size();
					DynArray<IGameSystem*>& systems = m_engine.m_gameSystemsByUpdateOrder;
					
					for(uint32 i=0; i<numDespawnedEntities; ++i)
					{
						for(uint32 s=0; s<numSystems; ++s)
						{
							systems[s]->ReleaseEntity(despawnedEntities[i]);
						}
					}
				}
				
				// perform spawning
				{
					EntitySpawnContext ctx;
					ctx.m_systems = gameSystemsProvider;
					ctx.m_entityIDGenerator = &m_engine.m_endityIDGenerator;
					
					fur::ExecuteCommands(&spawnCommands, &ctx);
				}
			}
			
			CameraSystem* cameraSystem = gameSystemsProvider.GetSystem<CameraSystem>();
			SpriteSystem* spriteSystem = gameSystemsProvider.GetSystem<SpriteSystem>();
			TiledTerrainSystem* terrainSystem = gameSystemsProvider.GetSystem<TiledTerrainSystem>();
			
			Matrix4x4* instanceMatrices = reinterpret_cast<Matrix4x4*>(m_engine.m_testInstanceMatrices->GetContent());
			Vector3* instanceColors = reinterpret_cast<Vector3*>(m_engine.m_testInstanceColors->GetContent());
			
			uint32 numInstances = 0;
			
			{
				const math::Vector3 playerPos = cameraSystem->GetAt();
				
				CollectTileRenderingContext tileCtx;
				tileCtx.m_cameraPosX = (int32)playerPos.x;
				tileCtx.m_cameraPosY = (int32)playerPos.y;
				
				CollectTileRenderingOutput tileOut;
				tileOut.m_matrices = {instanceMatrices, 1024};
				tileOut.m_colors = {instanceColors, 1024};
				tileOut.m_numTilesCollected = 0;
			
				terrainSystem->CollectTileRendering(tileCtx, tileOut);
				
				FUR_ASSERT(tileOut.m_numTilesCollected > 0);
				
				numInstances = Min(tileOut.m_numTilesCollected, 1024u);
			}
			
			//uint32 calculatedMatrices = debugMeshRenderSystem->CalculateMatrices(gameSystemsProvider, instanceMatrices, numInstances, 0);
			//FUR_ASSERT(calculatedMatrices > 0);
			//debugMeshRenderSystem->GetColors(instanceColors, numInstances, 0);
			
			// get sprite instances
			const uint32 numSprites = spriteSystem->NumSprites();
			
			SpriteInstance* spriteInstances = reinterpret_cast<SpriteInstance*>(m_engine.m_testSpriteInstances->GetContent());
			spriteSystem->GetSpriteInstances(gameSystemsProvider, spriteInstances, numSprites);
			
			// create frame structure
			rend::Frame frame;
			frame.m_camera.m_fov = cameraSystem->GetFov();
			frame.m_camera.m_eye = cameraSystem->GetEye();
			frame.m_camera.m_at = cameraSystem->GetAt();
			frame.m_camera.m_up = cameraSystem->GetUp();
			frame.m_objectsData = &sceneData;
			frame.m_objectsCount = 0;
			frame.m_tiles = ArrayView<rend::TileInfo>();
			
			// mesh stuff
			const ResourceID meshResID = BuildResourceID(c_meshesPaths[0]);	// todo: cache it
			bool meshResFound = m_engine.m_resources.m_meshes.count(meshResID) != 0;
			
			frame.m_testMesh = meshResFound ? m_engine.m_resources.m_meshes[meshResID].get() : nullptr;
			frame.m_testInstanceMatrices = m_engine.m_testInstanceMatrices.get();
			frame.m_testInstanceColors = m_engine.m_testInstanceColors.get();
			frame.m_testAlbedoTexture = m_engine.m_albedoTexture.get();
			frame.m_testAmbientOcclusionTexture = m_engine.m_ambientOcclusionTexture.get();
			frame.m_testNumInstances = numInstances;
			
			// sprites stuff
			frame.m_testSpriteInstances = m_engine.m_testSpriteInstances.get();
			frame.m_testNumSpriteInstances = numSprites;
			
			m_engine.m_renderer.DrawFrame(&frame);
		}
		
		// this is the only things that should be done in DrawViewport
		// basically drawing next valid frame or waiting (working on fibers) if there's none available yet
		m_engine.m_renderer.PresentFrame(viewport.m_drawable, viewport.m_passDescriptor);
		
        return true;
    }
    
    virtual void Suspend()
    {
        
    }
    
    virtual void Resume()
    {
        
    }
    
    virtual void OnExternalCloseRequest()
    {
        
    }
    
    virtual void Shutdown()
    {
        //jobs::DeinitializeJobSystem();
		
		// deinitialize game systems
		{
			DynArray<game::IGameSystem*>& systems = m_engine.m_gameSystemsByUpdateOrder;
			
			game::ShutdownSystemContext ctx;
			
			for(int32 i=(int32)systems.size()-1; i>=0; --i)
			{
				systems[i]->Shutdown(ctx);
			}
			
			for(int32 i=(int32)systems.size()-1; i>=0; --i)
			{
				delete systems[i];
			}
		}
    }
    
    input::InputCollector& GetInputCollector() { return m_engine.m_inputCollector; }
    
private:
	bool LoadResources_TEMP();
	bool LoadTextures_TEMP();
	UniquePtr<gpu::Texture> LoadTexture(const char* path);
	
	GameEngine m_engine;
};

// main game loop, on fiber
void MainGameLoopJob(jobs::Context& ctx)
{
	std::this_thread::sleep_for(std::chrono::milliseconds(1));
}

/**************** FURBALL CAT GAME ENGINE ****************/

struct FurMainAppDesc
{
	uint32 m_width;
	uint32 m_height;
	const char* m_title;
};

struct FurGameEngineDesc
{
	FurMainAppDesc m_mainApp;
};

struct MemDebugInfo
{
	MemDebugInfo* m_next;
	MemDebugInfo* m_prev;
	const char* m_fileAndLine;
	uint32 m_size;
};

struct MemDefaultAllocatorInternals
{
	uint64 m_totalSize;
	uint32 m_numAllocs;
	MemDebugInfo m_rootDebugInfo;
};

struct FurGameEngineMemoryAllocators
{
	MemAllocator m_default;
	MemDefaultAllocatorInternals _defaultInternals;	// should be in cache when accessing m_default
};

struct FurGameEngineInitContext
{
	const char* m_depotPath;
};

// Furball Cat - Memory
MemBlock furMemoryAllocatorGeneral_alloc(const uint32 size, void* internals, const char* info)
{
	MemDefaultAllocatorInternals* inter = (MemDefaultAllocatorInternals*)internals;
	
	const uint32 debugSize = sizeof(MemDebugInfo) + size;
	
	inter->m_numAllocs += 1;
	inter->m_totalSize += debugSize;
	MemBlock blk = {malloc(debugSize), size};
	
	MemDebugInfo* debugInfo = (MemDebugInfo*)blk.m_ptr;
	
	debugInfo->m_size = size;
	debugInfo->m_prev = &inter->m_rootDebugInfo;
	debugInfo->m_next = inter->m_rootDebugInfo.m_next;
	debugInfo->m_fileAndLine = info;
	
	inter->m_rootDebugInfo.m_next = debugInfo;
	
	blk.m_ptr = (uint8*)blk.m_ptr + sizeof(MemDebugInfo);
	
	return blk;
}

void furMemoryAllocatorGeneral_dealloc(MemBlock blk, void* internals, const char* info)
{
	MemDefaultAllocatorInternals* inter = (MemDefaultAllocatorInternals*)internals;
	FUR_ASSERT(inter->m_numAllocs >= 0 && inter->m_totalSize >= blk.m_size);
	inter->m_totalSize -= blk.m_size;
	inter->m_numAllocs -= 1;
	
	blk.m_ptr = (uint8*)blk.m_ptr - sizeof(MemDebugInfo);
	
	MemDebugInfo* debugInfo = (MemDebugInfo*)blk.m_ptr;
	debugInfo->m_prev->m_next = debugInfo->m_next;
	
	free(blk.m_ptr);
}

bool furMemoryAllocatorGeneral_owns(void* ptr, void* internals)
{
	return true;
}

bool furValidateAllocatorGeneral(MemDefaultAllocatorInternals* inter)
{
	MemDebugInfo* debugInfo = inter->m_rootDebugInfo.m_next;
	if(debugInfo)
	{
		printf("MEMORY LEAK DETECTED - Missing FUR_FREE for:\n");
		while(debugInfo)
		{
			printf("%s\n", debugInfo->m_fileAndLine);
			debugInfo = debugInfo->m_next;
		}
	}
	
	return (inter->m_numAllocs == 0);
}

// Furball Cat - Init engine functions

const char* g_furLastDetailedError = "";

void furSetLastError(const char* error)
{
	g_furLastDetailedError = error;
}

const char* FurGetLastError()
{
	return g_furLastDetailedError;
}

bool furInitMemoryAllocators(FurGameEngineMemoryAllocators& allocators)
{
	// default memory allocator
	allocators._defaultInternals = {};
	allocators.m_default.m_internals = &allocators._defaultInternals;
	allocators.m_default.alloc_func = &furMemoryAllocatorGeneral_alloc;
	allocators.m_default.dealloc_func = &furMemoryAllocatorGeneral_dealloc;
	allocators.m_default.owns_func = &furMemoryAllocatorGeneral_owns;
	
	return true;
}

#if 0
enum FurResult
{
	FUR_RESULT_SUCCESS = 0,
	FUR_RESULT_ERROR_FILE_OPEN,
	FUR_RESULT_ERROR_SHADER_MODULE_CREATION,
	FUR_RESULT_ERROR_ENGINE_ALLOC,
	FUR_RESULT_ERROR_MEMORY,
	FUR_RESULT_ERROR_GLFW,
	FUR_RESULT_ERROR_MAIN_WINDOW,
	FUR_RESULT_ERROR_GPU,
};

const char* furGetResultDescription(FurResult res)
{
	switch(res)
	{
			FUR_CASE_ENUM_TO_CSTR(FUR_RESULT_SUCCESS, " - Success");
			FUR_CASE_ENUM_TO_CSTR(FUR_RESULT_ERROR_FILE_OPEN, " - Can't open file");
			FUR_CASE_ENUM_TO_CSTR(FUR_RESULT_ERROR_SHADER_MODULE_CREATION, " - Can't create shader module");
			FUR_CASE_ENUM_TO_CSTR(FUR_RESULT_ERROR_ENGINE_ALLOC, " - Can't allocate memory for engine structure");
			FUR_CASE_ENUM_TO_CSTR(FUR_RESULT_ERROR_MEMORY, " - Can't initialize memory allocators");
			FUR_CASE_ENUM_TO_CSTR(FUR_RESULT_ERROR_GLFW, " - Can't initialize GLFW");
			FUR_CASE_ENUM_TO_CSTR(FUR_RESULT_ERROR_MAIN_WINDOW, " - Can't create main application window");
			FUR_CASE_ENUM_TO_CSTR(FUR_RESULT_ERROR_GPU, " - Can't initialize GPU");
		default:
			break;
	}
	
	return "Unknown error";
}
#endif

struct FurGameEngine
{
	struct fr_app_t* pApp;
	struct fr_renderer_t* pRenderer;
	fp_physics_t* pPhysics;
	
	std::chrono::system_clock::time_point prevTimePoint;
	
	fp_physics_scene_t* pPhysicsScene;
};

// Furball Cat - Platform
bool furMainEngineInit(const FurGameEngineDesc& desc, FurGameEngine** ppEngine)
{
	FurGameEngine* pEngine = (FurGameEngine*)malloc(sizeof(FurGameEngine));
	memset(pEngine, 0, sizeof(FurGameEngine));
	
	fr_app_desc_t appDesc;
	appDesc.appTitle = desc.m_mainApp.m_title;
	appDesc.viewportWidth = desc.m_mainApp.m_width;
	appDesc.viewportHeight = desc.m_mainApp.m_height;
	
	fr_result_t res = fr_create_app(&appDesc, &pEngine->pApp, NULL);
	
	if(res == FR_RESULT_OK)
	{
		fr_renderer_desc_t rendererDesc;
		rendererDesc.pApp = pEngine->pApp;
		
		res = fr_create_renderer(&rendererDesc, &pEngine->pRenderer, NULL);
	}
	
	if(res == FR_RESULT_OK)
	{
		res = fp_init_physics(&pEngine->pPhysics, NULL) == 0 ? FR_RESULT_OK : FR_RESULT_PHYSICS_INIT_ERROR;
	}
	
	if(res == FR_RESULT_OK)
	{
		*ppEngine = pEngine;
	}
	else
	{
		furSetLastError(fr_get_last_error());
		free(pEngine);
		return false;
	}
	
	// create physics scene
	if(res == FR_RESULT_OK)
	{
		fp_physics_scene_create(pEngine->pPhysics, &pEngine->pPhysicsScene, NULL);
	}
	
	return true;
}

void furMainEngineLoop(FurGameEngine* pEngine)
{
	pEngine->prevTimePoint = std::chrono::system_clock::now();
	
	while(fr_update_app(pEngine->pApp))
	{
		std::chrono::system_clock::time_point timeNow = std::chrono::system_clock::now();
		std::chrono::duration<float> dt = timeNow - pEngine->prevTimePoint;
		pEngine->prevTimePoint = timeNow;
		
		fr_update_context_t ctx;
		ctx.dt = dt.count();
		fr_update_renderer(pEngine->pRenderer, &ctx);
		fr_draw_frame(pEngine->pRenderer);
	}
	
	fr_wait_for_device(pEngine->pRenderer);
}

bool furMainEngineTerminate(FurGameEngine* pEngine)
{
	// check for memory leaks
	//FUR_ASSERT(furValidateAllocatorGeneral(&pEngine->m_memory._defaultInternals));
	
	fp_physics_scene_release(pEngine->pPhysics, pEngine->pPhysicsScene, NULL);
	
	fp_release_physics(pEngine->pPhysics, NULL);
	fr_release_renderer(pEngine->pRenderer, NULL);
	fr_release_app(pEngine->pApp, NULL);
	
	free(pEngine);	// rest of the deallocations should happen through allocators
	
	return true;
}

FUR_MAIN
{
	FurGameEngineDesc desc;
	memZero(&desc, sizeof(desc));
	
	desc.m_mainApp.m_width = 1600;
	desc.m_mainApp.m_height = 900;
	desc.m_mainApp.m_title = "Duena";
	
	FurGameEngine* pEngine = NULL;
	
	// initialize most basic engine components
	bool initResult = furMainEngineInit(desc, &pEngine);
	if(!initResult)
	{
		printf("Engine initialization error. Last known error: %s.\n", FurGetLastError());
		return initResult;
	}
	
	// main engine loop
	furMainEngineLoop(pEngine);
	
	// terminate most basic engine components
	bool result = furMainEngineTerminate(pEngine);
	if(!result)
	{
		return 1;
	}
	
	return 0;
#if 0
    SimpleApp app;
    
    platform::SimpleAppDesc desc;
    desc.windowWidth = c_width;
    desc.windowHeight = c_height;
    desc.app = &app;
    desc.m_inputCollector = &app.GetInputCollector();
    
    return platform::RunSimpleApp(desc);
#endif
}

/**************** FURBALL CAT GAME ENGINE ****************/

// Gameplay code

void LoadOBJMeshDataIntoGeometryBuilder(rend::GeometryBuilder& builder, const objl::Loader& objLoader)
{
	uint32 numVertices = (uint32)objLoader.LoadedVertices.size();
	uint32 numIndices = (uint32)objLoader.LoadedIndices.size();
	
	rend::SimpleVertexLayout vertex;
	
	const auto& objVertices = objLoader.LoadedVertices;
	
	// get vertices
	for(uint32 i=0; i<numVertices; ++i)
	{
		const auto& objVertex = objVertices[i];
		
		vertex.m_position.x = objVertex.Position.X;
		vertex.m_position.y = objVertex.Position.Y;
		vertex.m_position.z = objVertex.Position.Z;
		
		vertex.m_normal.x = objVertex.Normal.X;
		vertex.m_normal.y = objVertex.Normal.Y;
		vertex.m_normal.z = objVertex.Normal.Z;
		
		vertex.m_uv.x = objVertex.TextureCoordinate.X;
		vertex.m_uv.y = objVertex.TextureCoordinate.Y;
		
		builder.AddVertex(vertex);
	}
	
	// get indices
	for(uint32 i=0; i<numIndices; ++i)
	{
		uint32 index = objLoader.LoadedIndices[i];
		builder.AddIndex(index);
	}
}

void LoadFBXMeshDataIntoGeometryBuilder(rend::GeometryBuilder& builder, const ofbx::IScene& scene)
{
	const int32 numMeshes = scene.getMeshCount();
	
	uint32 vertexIndex = 0;
	
	for(int32 i=0; i<numMeshes; ++i)
	{
		const ofbx::Mesh* mesh = scene.getMesh(i);
		const ofbx::Geometry* geometry = mesh->getGeometry();
		
		const int32 numVertices = geometry->getVertexCount();
		const ofbx::Vec3* vertices = geometry->getVertices();
		const ofbx::Vec3* normals = geometry->getNormals();
		const ofbx::Vec2* uvs = geometry->getUVs();
		
		rend::SimpleVertexLayout vertex;
		
		for(int32 iv=0; iv<numVertices; ++iv)
		{
			vertex.m_position.x = vertices[iv].x;
			vertex.m_position.y = vertices[iv].y;
			vertex.m_position.z = vertices[iv].z;
			
			vertex.m_normal.x = normals[iv].x;
			vertex.m_normal.y = normals[iv].y;
			vertex.m_normal.z = normals[iv].z;
			
			vertex.m_uv.x = uvs[iv].x;
			vertex.m_uv.y = uvs[iv].y;
			
			builder.AddVertex(vertex);
			builder.AddIndex(vertexIndex);
			
			++vertexIndex;
		}
	}
}

struct FBXAnimCurve
{
	DynArray<float> m_times;
	DynArray<float> m_values;
};

struct FBXBoneInfo
{
	String m_name;
	String m_parentName;
	
	FBXAnimCurve m_translation[3];
	FBXAnimCurve m_rotation[3];
	FBXAnimCurve m_scale[3];
};

struct FBXRig
{
	DynArray<String> m_jointNames;
	DynArray<int16> m_parents;
	
	DynArray<math::Transform> m_referencePose;
};

struct FBXAnimation
{
	DynArray<String> m_jointNames;
	
	DynArray<uint32> m_translationOffsets;
	DynArray<uint32> m_translationLengths;
	
	DynArray<uint32> m_rotationOffsets;
	DynArray<uint32> m_rotationLengths;
	
	DynArray<uint32> m_translationTimes;
	DynArray<math::Vector3> m_translations;
	
	DynArray<uint32> m_rotationTimes;
	DynArray<math::EulerAngles> m_rotations;
};

void FillFBXAnimCurve(FBXAnimCurve& curve, const ofbx::AnimationCurve* fbxCurve)
{
	const int32 numKeys = fbxCurve->getKeyCount();
	const int64* keyTimes = fbxCurve->getKeyTime();
	const float* keyValues = fbxCurve->getKeyValue();
	
	curve.m_times.resize(numKeys);
	curve.m_values.resize(numKeys);
	
	for(int32 ik=0; ik<numKeys; ++ik)
	{
		curve.m_times[ik] = ofbx::fbxTimeToSeconds(keyTimes[ik]);
		curve.m_values[ik] = keyValues[ik];
	}
}

void FBXAnimCurve_CollectFramesToSample(const FBXAnimCurve& curve, DynArray<uint32>& frames)
{
	constexpr float fps = 30.0f;
	
	const uint32 size = (uint32)curve.m_times.size();
	
	for(uint32 it=0; it<size; ++it)
	{
		const float t = curve.m_times[it];
		const uint32 f = t * fps;
		
		bool found = false;
		for(uint32 idx=0; idx<frames.size(); ++idx)
		{
			if(frames[idx] == f)
			{
				found = true;
				break;
			}
		}
		
		if(!found)
		{
			frames.push_back(f);
		}
	}
}

void FBXAnimCurve_SampleAtFrame(const FBXAnimCurve* curve, uint32 size, float* result, uint32 frame)
{
	constexpr float frequency = 1.0f / 30.0f;
	
	const float time = frame * frequency;
	
	for(uint32 i=0; i<size; ++i)
	{
		const uint32 timesSize = (uint32)curve[i].m_times.size();
		
		uint32 idx = 0;
		while(curve[i].m_times[idx] < time && idx < timesSize)
		{
			++idx;
		}
		
		const uint32 upperIdx = math::Min(idx + 1, timesSize);
		
		if(idx == upperIdx)
		{
			const float value = curve[i].m_values[idx];
			result[i] = value;
		}
		else
		{
			const float lowerTime = curve[i].m_times[idx];
			const float upperTime = curve[i].m_times[upperIdx];
			
			const float alpha = (time - lowerTime) / (upperTime - lowerTime);
			
			const float lowerValue = curve[i].m_values[idx];
			const float upperValue = curve[i].m_values[upperIdx];
			const float value = math::Lerp(lowerValue, upperValue, alpha);
			result[i] = value;
		}
	}
}

void LoadFBXAnimationDataIntoAnimation(FBXRig& rig, FBXAnimation& animation, const ofbx::IScene& scene)
{
	HashMap<String, FBXBoneInfo*> bones;
	
	const int32 numAnimStacks = scene.getAnimationStackCount();
	for(int32 i=0; i<numAnimStacks; ++i)
	{
		const ofbx::AnimationStack* animStack = scene.getAnimationStack(i);
		const ofbx::AnimationLayer* layer = animStack->getLayer(0);	// usually there's only one layer, unless two animations are blended together
		const int32 numCurveNodes = layer->getCurveNodeCount();
		
		for(int32 icn=0; icn<numCurveNodes; ++icn)
		{
			const ofbx::AnimationCurveNode* curveNode = layer->getCurveNode(icn);
			const ofbx::Object* bone = curveNode->getBone();
			if(bone)
			{
				if(!(strcmp(bone->name, "Armature") == 0))		// Blender adds 'Armature' as root bone
				{
					if(bones.count(bone->name) == 0)
					{
						FBXBoneInfo* info = new FBXBoneInfo();
						bones[bone->name] = info;
						info->m_name = String(bone->name);
						
						const ofbx::Object* parentBone = bone->getParent();
						if(parentBone)
						{
							info->m_parentName = String(parentBone->name);
						}
					}
					
					FBXBoneInfo* info = bones[bone->name];
					
					String propertyName;
					const int32 propNameSize = curveNode->getBoneLinkPropertySize();
					if(propNameSize < 128)
					{
						char buffer[128];
						curveNode->getBoneLinkProperty(buffer, propNameSize);
						buffer[propNameSize] = '\0';
						propertyName = String(buffer);
					}
					
					const int32 numCurves = math::Min(curveNode->getCurveCount(), 3);
					
					if(propertyName == "Lcl Translation")
					{
						for(int32 ic=0; ic<numCurves; ++ic)
						{
							FillFBXAnimCurve(info->m_translation[ic], curveNode->getCurve(ic));
						}
					}
					else if(propertyName == "Lcl Rotation")
					{
						for(int32 ic=0; ic<numCurves; ++ic)
						{
							FillFBXAnimCurve(info->m_rotation[ic], curveNode->getCurve(ic));
						}
					}
					else if(propertyName == "Lcl Scale")
					{
						for(int32 ic=0; ic<numCurves; ++ic)
						{
							FillFBXAnimCurve(info->m_scale[ic], curveNode->getCurve(ic));
						}
					}
				}
			}
		}
	}
	
	if(!bones.empty())
	{
		// preparing stuff
		DynArray<const FBXBoneInfo*> rigBones;
		rigBones.reserve(bones.size());
		for(const auto& it : bones)
		{
			const FBXBoneInfo* info = it.second;
			rigBones.push_back(info);
		}
		
		DynArray<const FBXBoneInfo*> sortedRigBones;
		sortedRigBones.reserve(rigBones.size());
	
		DynArray<String> parents;
		parents.reserve(bones.size());
		
		parents.push_back("Armature");
		while(!parents.empty())
		{
			const String& parent = parents.front();
			
			for(uint32 i=0; i<rigBones.size(); ++i)
			{
				if(rigBones[i]->m_parentName == parent)
				{
					sortedRigBones.push_back(rigBones[i]);
					parents.push_back(rigBones[i]->m_name);
				}
			}
			
			parents.erase(parents.begin());
		}
		
		// rig
		rig.m_parents.resize(rigBones.size());
		rig.m_jointNames.resize(rigBones.size());
		rig.m_referencePose.resize(rigBones.size());
		
		if(!sortedRigBones.empty())
		{
			for(uint32 i=0; i<sortedRigBones.size(); ++i)
			{
				rig.m_jointNames[i] = sortedRigBones[i]->m_name;
				
				math::Vector3 position = {0.0f, 0.0f, 0.0f};
				
				const FBXAnimCurve* translationCurves = sortedRigBones[i]->m_translation;
				if(!translationCurves[0].m_values.empty())
				{
					position.x = translationCurves[0].m_values[0];
				}
				if(!translationCurves[1].m_values.empty())
				{
					position.y = translationCurves[1].m_values[0];
				}
				if(!translationCurves[2].m_values.empty())
				{
					position.z = translationCurves[2].m_values[0];
				}
				
				math::EulerAngles rotation = {0.0f, 0.0f, 0.0f};
				
				const FBXAnimCurve* rotationCurves = sortedRigBones[i]->m_rotation;
				if(!rotationCurves[0].m_values.empty())
				{
					rotation.yaw = rotationCurves[0].m_values[0];
				}
				if(!rotationCurves[1].m_values.empty())
				{
					rotation.pitch = rotationCurves[1].m_values[0];
				}
				if(!rotationCurves[2].m_values.empty())
				{
					rotation.roll = rotationCurves[2].m_values[0];
				}
				
				rig.m_referencePose[i].translation = math::CreateVector4(position, 0.0f);
				rig.m_referencePose[i].rotation = math::CreateQuaternionFromEulerAnglesYZPXRY(rotation);
			}
			
			for(int32 i=0; i<sortedRigBones.size(); ++i)
			{
				const String& parentName = sortedRigBones[i]->m_parentName;
				int16 parentIndex = -1;
				
				for(int32 p=i-1; p>=0; --p)
				{
					if(sortedRigBones[p]->m_name == parentName)
					{
						parentIndex = (int16)p;
						break;
					}
				}
				
				rig.m_parents[i] = parentIndex;
				
				// rotate root bones by -90 degrees around X axis (joints have different orientation than world, so we fix this by rotating root)
				if(parentIndex == -1)
				{
					rig.m_referencePose[i].rotation *= math::CreateQuaternionRotationAxis(1.0f, 0.0f, 0.0f, -90_deg);
				}
			}
			
			// animation
			{
				const uint32 numJoints = (uint32)rigBones.size();
				
				animation.m_jointNames.resize(numJoints);
				animation.m_translationOffsets.resize(numJoints);
				animation.m_translationLengths.resize(numJoints);
				animation.m_rotationOffsets.resize(numJoints);
				animation.m_rotationLengths.resize(numJoints);
				
				uint32 offset = 0;
				
				DynArray<uint32> tempTimes;
				
				for(uint32 i=0; i<numJoints; ++i)
				{
					animation.m_jointNames[i] = rigBones[i]->m_name;
					
					// translations
					{
						// collect frames to sample
						FBXAnimCurve_CollectFramesToSample(rigBones[i]->m_translation[0], tempTimes);
						FBXAnimCurve_CollectFramesToSample(rigBones[i]->m_translation[1], tempTimes);
						FBXAnimCurve_CollectFramesToSample(rigBones[i]->m_translation[2], tempTimes);
						
						// sort times
						std::sort(tempTimes.begin(), tempTimes.end(), [](uint32 a, uint32 b) { return a < b; } );
						
						// fill offsets and lengths
						animation.m_translationOffsets[i] = offset;
						animation.m_translationLengths[i] = (uint32)tempTimes.size();
						
						offset += (uint32)tempTimes.size();
						
						// sample
						for(uint32 f=0; f<tempTimes.size(); ++f)
						{
							const uint32 frame = tempTimes[f];
							float value[3];
							FBXAnimCurve_SampleAtFrame(rigBones[i]->m_translation, 3, value, frame);
						
							animation.m_translations.push_back({value[0], value[1], value[2]});
							animation.m_translationTimes.push_back(frame);
						}
						
						tempTimes.clear();
					}
					
					// rotations
					{
						// collect frames to sample
						FBXAnimCurve_CollectFramesToSample(rigBones[i]->m_rotation[0], tempTimes);
						FBXAnimCurve_CollectFramesToSample(rigBones[i]->m_rotation[1], tempTimes);
						FBXAnimCurve_CollectFramesToSample(rigBones[i]->m_rotation[2], tempTimes);
						
						// sort times
						std::sort(tempTimes.begin(), tempTimes.end(), [](uint32 a, uint32 b) { return a < b; } );
						
						// fill offsets and lengths
						animation.m_rotationOffsets[i] = offset;
						animation.m_rotationLengths[i] = (uint32)tempTimes.size();
						
						offset += (uint32)tempTimes.size();
						
						// sample
						for(uint32 f=0; f<tempTimes.size(); ++f)
						{
							const uint32 frame = tempTimes[f];
							float value[3];
							FBXAnimCurve_SampleAtFrame(rigBones[i]->m_rotation, 3, value, frame);
							
							animation.m_rotations.push_back({value[0], value[1], value[2]});
							animation.m_rotationTimes.push_back(frame);
						}
						
						tempTimes.clear();
					}
				}
			}
		}
		
		for(uint32 i=0; i<rigBones.size(); ++i)
		{
			delete rigBones[i];
		}
	}
}

ofbx::IScene* OpenScene_FBX(const char* path)
{
	FileStream file(path, FileStream::in | FileStream::binary | FileStream::ate);
	FUR_ASSERT(file);
	
	uint32 fileSize = (uint32)file.tellg();
	file.seekg(0, FileStream::beg);
	
	DynArray<char> fileData(fileSize);
	
	file.read(&fileData[0], fileSize);
	file.close();
	
	FUR_ASSERT(!fileData.empty());
	
	ofbx::IScene* scene = ofbx::load((const uint8*)fileData.data(), (int)fileData.size());
	FUR_ASSERT(scene);
	
	return scene;
}

bool LoadRig_FBX(anim::Rig& rig, const char* path)
{
	ofbx::IScene* scene = OpenScene_FBX(path);
	
	FBXRig fbxRig;
	FBXAnimation fbxAnimation;
	LoadFBXAnimationDataIntoAnimation(fbxRig, fbxAnimation, *scene);
	scene->destroy();
	
	uint32 numJoints = (uint32)fbxRig.m_jointNames.size();
	rig.m_jointNames.reserve(numJoints);
	rig.m_parents.reserve(numJoints);
	rig.m_referencePose.reserve(numJoints);
	
	for(uint32 i=0; i<numJoints; ++i)
	{
		rig.m_jointNames.push_back(BuildStringHash(fbxRig.m_jointNames[i].c_str()));
		rig.m_parents.push_back(fbxRig.m_parents[i]);
		rig.m_referencePose.push_back(fbxRig.m_referencePose[i]);
	}
	
	return true;
}

bool LoadAnimation_FBX(anim::Animation& animation, const char* path)
{
	ofbx::IScene* scene = OpenScene_FBX(path);
	
	FBXRig fbxRig;
	FBXAnimation fbxAnimation;
	LoadFBXAnimationDataIntoAnimation(fbxRig, fbxAnimation, *scene);
	scene->destroy();
	
	const uint32 numJoints = (uint32)fbxAnimation.m_rotationOffsets.size();
	const uint32 numTranslations = (uint32)fbxAnimation.m_translations.size();
	const uint32 numRotations = (uint32)fbxAnimation.m_rotations.size();
	
	animation.m_offsets.resize(numJoints);
	
	animation.m_translationTimes.resize(numTranslations);
	animation.m_translations.resize(numTranslations);
	
	animation.m_rotationTimes.resize(numRotations);
	animation.m_rotations.resize(numRotations);
	
	// todo: memcopy
	for(uint32 i=0; i<numJoints; ++i)
	{
		animation.m_offsets[i].m_translationOffset = fbxAnimation.m_translationOffsets[i];
		animation.m_offsets[i].m_translationLength = fbxAnimation.m_translationLengths[i];
		animation.m_offsets[i].m_rotationOffset = fbxAnimation.m_rotationOffsets[i];
		animation.m_offsets[i].m_rotationLength = fbxAnimation.m_rotationLengths[i];
	}
	
	for(uint32 i=0; i<numTranslations; ++i)
	{
		animation.m_translations[i] = fbxAnimation.m_translations[i];
		animation.m_translationTimes[i] = fbxAnimation.m_translationTimes[i];
	}
	
	for(uint32 i=0; i<fbxAnimation.m_rotations.size(); ++i)
	{
		const EulerAngles angles = fbxAnimation.m_rotations[i];
		Quaternion q = math::CreateQuaternionFromEulerAnglesYZPXRY(angles);
		animation.m_rotations[i] = q;
		animation.m_rotationTimes[i] = fbxAnimation.m_rotationTimes[i];
	}
	
	return true;
}

bool IsFileExtensionEqualTo(const char* path, const char* ext)
{
	const size_t pathLen = strlen(path);
	const size_t extLen = strlen(ext);
	
	if(strcmp(path + pathLen - extLen, ext) == 0)
	{
		return true;
	}
	
	return false;
}

bool LoadRegularMeshFromFile(gpu::Device* device, rend::Mesh& mesh, const char* path)
{
	rend::GeometryBuilder builder;
	
	if(IsFileExtensionEqualTo(path, ".obj"))
	{
		objl::Loader objLoader;
		if(objLoader.LoadFile(path) == false)
			return false;
		
		LoadOBJMeshDataIntoGeometryBuilder(builder, objLoader);
	}
	else if(IsFileExtensionEqualTo(path, ".fbx"))
	{
		ofbx::IScene* scene = OpenScene_FBX(path);
		
		LoadFBXMeshDataIntoGeometryBuilder(builder, *scene);
		
		scene->destroy();
	}
	else
	{
		return false;
	}
	
	// create vertex layout
	rend::VertexLayoutAttribute layout[3];
	layout[0] = rend::VertexLayoutAttribute::Position;
	layout[1] = rend::VertexLayoutAttribute::Normal;
	layout[2] = rend::VertexLayoutAttribute::UV;
	
	// build mesh chunk
	mesh.m_chunks.push_back(rend::MeshChunk());
	rend::MeshChunk& chunk = mesh.m_chunks[0];
	rend::BuildMeshChunk(builder, device, layout, 3, 0, chunk);
	
	return true;
}

bool SimpleApp::LoadResources_TEMP()
{
	gpu::Device* device = m_engine.m_renderer.GetDevice();
	
	for(uint32 i=0; i<ArraySize(c_meshesPaths); ++i)
	{
		UniquePtr<rend::Mesh> meshRes(new rend::Mesh());
		if(LoadRegularMeshFromFile(device, *meshRes, c_meshesPaths[i]) == false)
		{
			return false;
		}
		
		ResourceID meshResID = BuildResourceID(c_meshesPaths[i]);
		m_engine.m_resources.m_meshes[meshResID] = std::move(meshRes);
	}
	
	for(uint32 i=0; i<ArraySize(c_rigsPaths); ++i)
	{
		UniquePtr<anim::Rig> res(new anim::Rig());
		if(LoadRig_FBX(*res, c_rigsPaths[i]) == false)
		{
			return false;
		}
		
		ResourceID id = BuildResourceID(c_rigsPaths[i]);
		m_engine.m_resources.m_rigs[id] = std::move(res);
	}
	
	for(uint32 i=0; i<ArraySize(c_animationPaths); ++i)
	{
		UniquePtr<anim::Animation> res(new anim::Animation());
		if(LoadAnimation_FBX(*res, c_animationPaths[i]) == false)
		{
			return false;
		}
		
		ResourceID id = BuildResourceID(c_rigsPaths[i]);
		m_engine.m_resources.m_animations[id] = std::move(res);
	}
	
	// create instance matrices buffer
	{
		gpu::BufferDesc desc;
		desc.m_data = nullptr;
		desc.m_size = 1024 * sizeof(Matrix4x4);
		m_engine.m_testInstanceMatrices = device->CreateBuffer(desc);
	}
	
	// create instance colors buffer
	{
		{
			gpu::BufferDesc desc;
			desc.m_data = nullptr;
			desc.m_size = 1024 * sizeof(Vector3);
			m_engine.m_testInstanceColors = device->CreateBuffer(desc);
		}
	}
	
	return true;
}

bool SimpleApp::LoadTextures_TEMP()
{
	m_engine.m_albedoTexture = LoadTexture(c_albedoTexturePath);
	if(m_engine.m_albedoTexture == nullptr)
		return false;
	
	m_engine.m_ambientOcclusionTexture = LoadTexture(c_ambientOcclusionTexturePath);
	if(m_engine.m_ambientOcclusionTexture == nullptr)
		return false;
	
	return true;
}

UniquePtr<gpu::Texture> SimpleApp::LoadTexture(const char* path)
{
	UniquePtr<gpu::Texture> texture;
	
	std::vector<uint8> textureData;
	uint32 width = 0;
	uint32 height = 0;
	
	if(lodepng::decode(textureData, width, height, path) != 0)
		return nullptr;
	
	gpu::Device* device = m_engine.m_renderer.GetDevice();
	
	{
		gpu::TextureDesc desc;
		desc.m_width = width;
		desc.m_height = height;
		desc.m_format = gpu::PixelFormat::RGBA8Unorm;
		desc.m_mipmapped = false;
		desc.m_storageMode = gpu::StorageMode::Managed;
		desc.m_usage = gpu::TextureUsage::ShaderRead;
		texture = device->CreateTexture(desc);
		
		gpu::Region region {{0, 0, 0}, {width, height, 1}};
		texture->ReplaceRegion(region, 0, textureData.data(), width * 4);
	}
	
	return texture;
}
