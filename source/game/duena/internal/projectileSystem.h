#pragma once

namespace game
{
	class ProjectileSystem : public IGameSystem
	{
	public:
		void CreateProjectile(EntityID id, const math::Vector3& velocity);
		
		virtual void ReleaseEntity(EntityID id) override;
		virtual void Tick(const TickSystemContext& ctx, float dt) override;
		
	private:
		static const uint32 NUM_MAX_PROJECTILES = 1024;
		math::Vector4 m_projectileVelocities[NUM_MAX_PROJECTILES];
		EntityID m_entityIDs[NUM_MAX_PROJECTILES];
		float m_timeToDespawn[NUM_MAX_PROJECTILES];
		uint32 m_numProjectiles = 0;
	};
}
