#pragma once

#include "core/commandBuffer.h"

namespace game
{
	typedef uint32 EntityID;
	constexpr EntityID INVALID_ENTITY_ID = 0xFFFFFFFF;

	GAMEWORK_API uint32 NewGameSystemID();
	
	template<typename T>
	inline uint32 GameSystemID()
	{
		static uint32 id = NewGameSystemID();
		return id;
	};
	
	class IGameSystem;
	
	//--

	struct GAMEWORK_API GameSystemsProvider
	{
		template<typename T>
		T* GetSystem() const
		{
			return reinterpret_cast<T*>(m_systems[GameSystemID<T>()]);
		}
		
		ArrayView<IGameSystem*> m_systems;
	};
	
	struct AttachComponentContext
	{
		GameSystemsProvider m_systems;
	};

	//--
	
	class GAMEWORK_API EntityIDGenerator
	{
		public:
		EntityIDGenerator() : m_nextFreeID(0) {}
		EntityID GetNewID() { return m_nextFreeID++; }
		void FreeID(EntityID id) {}		// todo: implement
		
		private:
		fur::Atomic<EntityID> m_nextFreeID;
	};
	
	struct GAMEWORK_API EntitySpawnContext
	{
		// this is just for convenience
		template<typename T>
		T* GetSystem() const
		{
			return m_systems.GetSystem<T>();
		}
		
		EntityID GetNewEntityID() { return m_entityIDGenerator->GetNewID(); }
		
		GameSystemsProvider m_systems;
		EntityIDGenerator* m_entityIDGenerator;
	};
	
	class GAMEWORK_API EntitySpawnQueue
	{
	public:
		EntitySpawnQueue(fur::CommandEncoder commandEncoder);
		
		template<typename T>
		void EnqueueSpawn(const T& spawner)
		{
			// todo: add command encoders and buffer per thread to make it lockless and thread safe
			m_commandEncoder.Commit(spawner);
		}
		
	private:
		fur::CommandEncoder m_commandEncoder;
	};
	
	class GAMEWORK_API EntityDespawnQueue
	{
	public:
		EntityDespawnQueue(EntityID* idArray, uint32* count, uint32 capacity)
			: m_idArray(idArray), m_count(count), m_capacity(capacity)
		{
			
		}
		
		void Despawn(EntityID id)
		{
			// todo: add thread-safety
			FUR_ASSERT(*m_count < m_capacity);
			m_idArray[*m_count] = id;
			++(*m_count);
		}
		
	private:
		EntityID* m_idArray;
		uint32* m_count;
		uint32 m_capacity;
	};
	
	//--
	
	struct TickSystemContext
	{
		float GetAction(uint32 playerIndex, uint32 actionID) const
		{
			return playerIndex == 0 ? m_actionsPlayer1[actionID] : m_actionsPlayer2[actionID];
		}
		
		// this is just for convenience
		template<typename T>
		T* GetSystem() const
		{
			return m_systems.GetSystem<T>();
		}
		
		template<typename T>
		void Spawn(const T& info) const
		{
			m_spawn->EnqueueSpawn(info);
		}
		
		void Despawn(EntityID id) const
		{
			m_despawn->Despawn(id);
		}
		
		ArrayView<float> m_actionsPlayer1;
		ArrayView<float> m_actionsPlayer2;
		GameSystemsProvider m_systems;
		EntitySpawnQueue* m_spawn;
		EntityDespawnQueue* m_despawn;
	};
	
	struct InitializeSystemContext
	{
		
	};
	
	struct ShutdownSystemContext
	{
		
	};
	
	class GAMEWORK_API IGameSystem
	{
	public:
		virtual ~IGameSystem() {}
		
		virtual void Initialize(const InitializeSystemContext& ctx) {}
		virtual void Shutdown(const ShutdownSystemContext& ctx) {}
		
		virtual void Tick(const TickSystemContext& ctx, float dt) {};
		
		// there's only virtual release function, as creation creation is done through spawning system,
		// where each component description can create component instances for given entity ID in systems
		virtual void ReleaseEntity(EntityID id) {}
	};
}
