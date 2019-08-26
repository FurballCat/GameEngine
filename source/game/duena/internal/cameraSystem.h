#pragma once

namespace game
{
	class CameraSystem : public IGameSystem
	{
	public:
		void AddEntityToFollow(EntityID id);
		
		virtual void Initialize(const InitializeSystemContext& ctx) override;
		virtual void Tick(const TickSystemContext& ctx, float dt) override;
		virtual void ReleaseEntity(EntityID id) override;
		
		math::Vector3 GetEye() const { return m_eye; }
		math::Vector3 GetAt() const { return m_at; }
		math::Vector3 GetUp() const { return m_up; }
		float GetFov() const { return m_fov; }
		
	private:
		static const uint32 NUM_MAX_ENTITIES_TO_FOLLOW = 32;
		EntityID m_entityToFollowIDs[NUM_MAX_ENTITIES_TO_FOLLOW];
		uint32 m_numEntitiesFollowed = 0;
		
		math::Vector3 m_eye = {0, 0, 0};
		math::Vector3 m_at = {1, 0, 0};
		math::Vector3 m_up = {0, 0, 1};
		float m_fov = 10.0f;
	};
}
