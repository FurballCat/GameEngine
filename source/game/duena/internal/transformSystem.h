#pragma once

namespace game
{
	// test code
	class TransformSystem : public IGameSystem
	{
	public:
		void CreateTransform(EntityID id, const math::Transform& desc) { m_transforms[id] = desc; }
		virtual void ReleaseEntity(EntityID id) override { m_transforms.erase(id); }
		math::Transform& GetTransformFor(EntityID id) { return m_transforms[id]; }
		const math::Transform& GetTransformFor(EntityID id) const { return m_transforms.find(id)->second; }
		
	private:
		HashMap<EntityID, math::Transform> m_transforms;
		
	};
}
