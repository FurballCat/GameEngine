#include "pch.h"
#include "projectileSystem.h"
#include "transformSystem.h"

using namespace game;

void game::ProjectileSystem::CreateProjectile(EntityID id, const math::Vector3& velocity)
{
	FUR_ASSERT(m_numProjectiles < NUM_MAX_PROJECTILES);
	
	uint32 i = m_numProjectiles;
	++m_numProjectiles;
	
	m_projectileVelocities[i] = math::CreateVector4(velocity, 0.0f);
	m_entityIDs[i] = id;
	m_timeToDespawn[i] = 4.0f;
}

void game::ProjectileSystem::ReleaseEntity(EntityID id)
{
	constexpr uint32 invalidIndex = 0xFFFFFFFF;
	uint32 removeIndex = invalidIndex;
	for(uint32 i=0; i<m_numProjectiles; ++i)
	{
		if(m_entityIDs[i] == id)
		{
			removeIndex = i;
			break;
		}
	}
	
	if(removeIndex == invalidIndex)
		return;
	
	FUR_ASSERT(removeIndex != invalidIndex);
	FUR_ASSERT(m_numProjectiles > 0);
	
	--m_numProjectiles;
	uint32 lastIndex = m_numProjectiles;
	
	if(lastIndex != removeIndex)
	{
		m_entityIDs[removeIndex] = m_entityIDs[lastIndex];
		m_projectileVelocities[removeIndex] = m_projectileVelocities[lastIndex];
		m_timeToDespawn[removeIndex] = m_timeToDespawn[lastIndex];
	}
}

void game::ProjectileSystem::Tick(const TickSystemContext& ctx, float dt)
{
	TransformSystem* sysTransform = ctx.GetSystem<TransformSystem>();
	
	for(uint32 i=0; i<m_numProjectiles; ++i)
	{
		EntityID id = m_entityIDs[i];
		if(m_timeToDespawn[i] <= 0.0f)
		{
			ctx.Despawn(id);
		}
		else
		{
			math::Transform& transform = sysTransform->GetTransformFor(id);
			const math::Vector4& velocity = m_projectileVelocities[i];
			
			transform.translation += velocity * dt;
			
			m_timeToDespawn[i] -= dt;
		}
	}
}
