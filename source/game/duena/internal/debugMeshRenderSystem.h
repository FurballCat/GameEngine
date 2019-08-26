#pragma once

namespace game
{
	// test code
	class DebugMeshRenderSystem : public IGameSystem
	{
	public:
		void VisualizeEntity(EntityID id, float scale, math::Vector3 color);
		
		virtual void ReleaseEntity(EntityID id) override;
		
		// returns number of matrices calculated
		uint32 CalculateMatrices(GameSystemsProvider systems, math::Matrix4x4* matrices, uint32 numMatrices, uint32 startIndex) const;
		uint32 GetColors(math::Vector3* colors, uint32 numColors, uint32 startIndex) const;
		uint32 NumMeshes() const { return m_numMeshes; }
		
	private:
		static const uint32 NUM_MAX_MESHES = 1024;
		EntityID m_entityIDs[NUM_MAX_MESHES];
		float m_scales[NUM_MAX_MESHES];
		math::Vector3 m_colors[NUM_MAX_MESHES];
		uint32 m_numMeshes = 0;
		
	};
}
