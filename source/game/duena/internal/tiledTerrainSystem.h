#pragma once

namespace game
{
	struct TilePosition
	{
		int32 x;
		int32 y;
	};
	
	struct WorldGeneratorContext
	{
		uint32 m_width;
		uint32 m_height;
		uint8 m_visualVariety;
		uint8 m_elevationVariety;
	};
	
	struct CollectTileRenderingContext
	{
		int32 m_cameraPosX;
		int32 m_cameraPosY;
	};
	
	struct CollectTileRenderingOutput
	{
		ArrayView<math::Matrix4x4> m_matrices;
		ArrayView<math::Vector3> m_colors;
		uint32 m_numTilesCollected = 0;
	};
	
	class TiledTerrainSystem : public IGameSystem
	{
	public:
		void GenerateWorld(const WorldGeneratorContext& ctx);
		
		virtual void ReleaseEntity(EntityID id) override;
		virtual void Tick(const TickSystemContext& ctx, float dt) override;
		
		void CollectTileRendering(const CollectTileRenderingContext& ctx, CollectTileRenderingOutput& output);
		
	private:
		DynArray<TilePosition> m_positions;
		DynArray<uint8> m_elevationAndOrientation;
		DynArray<uint8> m_visualIndex;	// refers to m_meshes
		DynArray<uint8> m_colors;
		DynArray<ResourceID> m_meshes;
		
		uint32 m_width = 0;
		uint32 m_height = 0;
	};
}
