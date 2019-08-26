#pragma once

namespace game
{
	// this might end up in common folder
	class RenderingSystem : public IGameSystem
	{
	public:
		void AddStaticMesh(EntityID id, const String& path, const math::Matrix4x4& matrix);
		
		virtual void Tick(const TickSystemContext& ctx, float dt) override;
		
	private:
		static const uint32 NUM_MAX_MESHES = 1024;
		EntityID m_entityIDs[NUM_MAX_MESHES];
		math::Matrix4x4 m_matrices[NUM_MAX_MESHES];
		uint32 m_meshIndex[NUM_MAX_MESHES];
	};
}
