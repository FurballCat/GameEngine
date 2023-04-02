/* Copyright (c) Furball Cat */

#include "world.h"
#include "ccore/public.h"
#include "ccore/buffer.h"
#include "canim/public.h"
#include <string.h>

#define LEVEL_HEAP_MEMORY_CAPACITY 16 * 1024 * 1024
#define MAX_TYPE_FACTORIES 128

typedef struct fg_world_global_t
{
	i32 stuff;
} fg_world_global_t;

fg_world_global_t g_worldExtended;

typedef struct fg_type_factory_register_t
{
	FcStringId typeNames[MAX_TYPE_FACTORIES];
	FcGameObjectFactory factories[MAX_TYPE_FACTORIES];
	i32 numFactories;
} fg_type_factory_register_t;

fg_type_factory_register_t g_typeFactories;

void fcGameObjectFactoryRegisterNew(FcStringId typeName, FcGameObjectFactory factory)
{
	FUR_ASSERT(g_typeFactories.numFactories < MAX_TYPE_FACTORIES);
	
	const i32 idx = g_typeFactories.numFactories;
	g_typeFactories.factories[idx] = factory;
	g_typeFactories.typeNames[idx] = typeName;
	g_typeFactories.numFactories++;
}

const FcGameObjectFactory* fg_type_factory_find(FcStringId typeName)
{
	for(i32 i=0; i<g_typeFactories.numFactories; ++i)
	{
		if(g_typeFactories.typeNames[i] == typeName)
		{
			return &g_typeFactories.factories[i];
		}
	}
	
	FUR_ASSERT(false);	// can't find matching type factory
	
	return NULL;
}

i32 fg_spawn_info_find_prop_index(const fg_spawn_info_properties_t* props, FcStringId name)
{
	for(i32 i=0; i<props->num; ++i)
	{
		if(props->names[i] == name)
			return i;
	}
	
	return -1;
}

f32 fg_spawn_info_get_float(const FcSpawnInfo* info, FcStringId name, f32 defaultValue)
{
	const i32 idx = fg_spawn_info_find_prop_index(&info->props, name);
	
	if(idx != -1)
		return info->props.values[idx].asFloat;
	
	return defaultValue;
}

i32 fg_spawn_info_get_int(const FcSpawnInfo* info, FcStringId name, i32 defaultValue)
{
	const i32 idx = fg_spawn_info_find_prop_index(&info->props, name);
	
	if(idx != -1)
		return info->props.values[idx].asInt32;
	
	return defaultValue;
}

FcStringId fg_spawn_info_get_string_hash(const FcSpawnInfo* info, FcStringId name, FcStringId defaultValue)
{
	const i32 idx = fg_spawn_info_find_prop_index(&info->props, name);
	
	if(idx != -1)
		return info->props.values[idx].asStringHash;
	
	return defaultValue;
}

void fcWorldInit(FcWorld* world, FcAllocator* pAllocCallbacks)
{
	// init by zero
	memset(world, 0, sizeof(FcWorld));
	
	world->levelHeap = FUR_ALLOC_AND_ZERO(sizeof(FcMemRelHeapPool), 0, FC_MEMORY_SCOPE_GAME, pAllocCallbacks);
	
	// allocate level heap
	world->levelHeap->buffer = FUR_ALLOC(LEVEL_HEAP_MEMORY_CAPACITY, 8, FC_MEMORY_SCOPE_GAME, pAllocCallbacks);
	world->levelHeap->freePtr = world->levelHeap->buffer;
	world->levelHeap->capacity = LEVEL_HEAP_MEMORY_CAPACITY;
	world->levelHeap->size = 0;
	
	// allocate update buckets
	fcArrayAllocAndZero(&world->buckets[FG_UPDATE_BUCKET_CHARACTERS], FcGameObjectHandle, 256, 8, FC_MEMORY_SCOPE_GAME, pAllocCallbacks);

	// allocate resource maps
	fcMapAllocAndZero(&world->resources.scripts, FcStringId, const FcBinaryBuffer, 512, 0, FC_MEMORY_SCOPE_GAME, pAllocCallbacks);
	fcMapAllocAndZero(&world->resources.animations, FcStringId, const FcAnimClip*, 1024, 0, FC_MEMORY_SCOPE_GAME, pAllocCallbacks);
	fcMapAllocAndZero(&world->resources.rigs, FcStringId, const FcRig*, 128, 0, FC_MEMORY_SCOPE_GAME, pAllocCallbacks);
	fcMapAllocAndZero(&world->resources.meshes, FcStringId, const FcRenderProxy*, 2048, 0, FC_MEMORY_SCOPE_GAME, pAllocCallbacks);
}

void fcWorldRelease(FcWorld* world, FcAllocator* pAllocCallbacks)
{
	// release level heap
	FUR_FREE(world->levelHeap->buffer, pAllocCallbacks);
	FUR_FREE(world->levelHeap, pAllocCallbacks);
	
	// release update buckets
	fcArrayFree(&world->buckets[FG_UPDATE_BUCKET_CHARACTERS], pAllocCallbacks);
	
	// release resources
	for(u32 i=0; i<world->resources.animations.num; ++i)
	{
		fcAnimClipRelease((FcAnimClip*)world->resources.animations.elems[i], pAllocCallbacks);
	}
	
	for(u32 i=0; i<world->resources.rigs.num; ++i)
	{
		fcRigRelease((FcRig*)world->resources.rigs.elems[i], pAllocCallbacks);
	}
	
	for(u32 i=0; i<world->resources.scripts.num; ++i)
	{
		fcBinaryBufferRelease((FcBinaryBuffer*)world->resources.scripts.elems[i], pAllocCallbacks);
	}

	// release resource maps
	fcMapFree(&world->resources.scripts, pAllocCallbacks);
	fcMapFree(&world->resources.animations, pAllocCallbacks);
	fcMapFree(&world->resources.rigs, pAllocCallbacks);
	fcMapFree(&world->resources.meshes, pAllocCallbacks);
}

void fcWorldUpdate(FcWorld* world, FcWorldUpdateCtx* ctx, FcUpdateBucket bucket)
{
	FcGameObjectInfoStorage* info = &world->gameObjects;
	
	// scheduled spawn game objects
	{
		for(i32 i=info->numInit; i<info->num; ++i)
		{
			FcGameObject* gameObject = info->ptr[i];
			if(gameObject != NULL)
			{
				//const FcGameObjectFactory* factory = fg_type_factory_find(info->spawner[i]->typeName);
				
				// game object can allocate its memory on level heap, through this stack allocator
				FcAllocator levelHeapAlloc = fcMemRelHeapGetAllocator(world->levelHeap);
				
				FcGameObjectInitCtx initCtx = {0};
				initCtx.info = &info->spawner[i]->info;
				initCtx.resources = &world->resources;
				initCtx.systems = &world->systems;
				initCtx.stackAlloc = &levelHeapAlloc;
				
				gameObject->fn->init(gameObject, &initCtx);
				
				// reserve memory that is actually used by the game object
				// ...
			}
		}
		
		info->numInit = info->num;
	}
	
	FcGameObjectUpdateCtx updateCtx = {0};
	updateCtx.dt = ctx->dt;
	
	// go through all update buckets
	for(i32 i=0; i<FG_UPDATE_BUCKET_COUNT; ++i)
	{
		FcArrayGameObjectHandles* bucket = &world->buckets[i];
		
		// go through all game objects within a bucket
		for(i32 idxHandle=0; idxHandle<bucket->num; ++i)
		{
			FcGameObjectHandle handle = bucket->data[idxHandle];
			
			// get game object info
			FcGameObject* gameObject = info->ptr[handle.index];
			
			// update game object
			gameObject->fn->update(gameObject, &updateCtx);
		}
	}
}

void* fg_stack_alloc(fg_stack_allocator_t* allocator, i32 size)
{
	FUR_ASSERT(allocator->size + size <= allocator->capacity);
	void* ptr = allocator->ptr;
	allocator->ptr = (u8*)allocator->ptr + size;
	allocator->size += size;
	
	memset(ptr, 0, size);
	
	return ptr;
}

const FcAnimClip* fcResourceRegisterFindAnimClip(const FcResourceRegister* reg, FcStringId name)
{
	const FcAnimClip* result = (const FcAnimClip*)fcMapFind(&reg->animations, &name);
	FUR_ASSERT(result);
	return result;
}

const FcBinaryBuffer* fcResourceRegisterFindScript(const FcResourceRegister* reg, FcStringId name)
{
	const FcBinaryBuffer* result = (const FcBinaryBuffer*)fcMapFind(&reg->scripts, &name);
	FUR_ASSERT(result);
	return result;
}

const FcRig* fcResourceRegisterFindRig(const FcResourceRegister* reg, FcStringId name)
{
	const FcRig* result = (const FcRig*)fcMapFind(&reg->rigs, &name);
	FUR_ASSERT(result);
	return result;
}

const FcRenderProxy* fcResourceRegisterFindMesh(const FcResourceRegister* reg, FcStringId name)
{
	const FcRenderProxy* result = (const FcRenderProxy*)fcMapFind(&reg->meshes, &name);
	FUR_ASSERT(result);
	return result;
}

void fcResourceRegisterAddAnimClip(FcResourceRegister* reg, FcStringId name, const FcAnimClip* res)
{
	fcMapInsert(&reg->animations, &name, &res);
}

void fcResourceRegisterAddScript(FcResourceRegister* reg, FcStringId name, const FcBinaryBuffer* res)
{
	fcMapInsert(&reg->scripts, &name, &res);
}

void fcResourceRegisterAddRig(FcResourceRegister* reg, FcStringId name, const FcRig* res)
{
	fcMapInsert(&reg->rigs, &name, &res);
}

void fcResourceRegisterAddMesh(FcResourceRegister* reg, FcStringId name, const FcRenderProxy* res)
{
	fcMapInsert(&reg->meshes, &name, &res);
}

FcGameObjectHandle fg_game_object_storage_find_free_handle(FcGameObjectInfoStorage* storage)
{
	FcGameObjectHandle handle = {0};
	handle.index = -1;
	
	// find free storage slot
	for(i32 i=0; i<MAX_GAME_OBJECTS_SPAWNED; ++i)
	{
		if(storage->ptr[i] == NULL)
		{
			handle.index = i;
			break;
		}
	}
	
	return handle;
}

FcGameObjectHandle fcSpawn(const FcSpawner* spawner, FcWorld* world)
{
	const FcGameObjectFactory* factory = fg_type_factory_find(spawner->typeName);
	
	FcAllocator alloc = fcMemRelHeapGetAllocator(world->levelHeap);
	FcGameObject* gameObject = FUR_ALLOC_AND_ZERO(factory->memoryMaxSize, 0, FC_MEMORY_SCOPE_GAME, &alloc);
	gameObject->fn = &factory->fn;
	gameObject->name = spawner->name;
	
	FcGameObjectHandle handle = fg_game_object_storage_find_free_handle(&world->gameObjects);
	FUR_ASSERT(handle.index != -1);
	
	handle.name = spawner->name;
	
	FcGameObjectInfoStorage* info = &world->gameObjects;
	info->ptr[handle.index] = gameObject;
	info->spawner[handle.index] = spawner;
	
	world->gameObjects.num++;
	
	return handle;
}

void fcDespawn(FcGameObjectHandle handle, FcWorld* world)
{
	// todo: implement
}


