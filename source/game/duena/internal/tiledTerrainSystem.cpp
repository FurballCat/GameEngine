#include "pch.h"
#include "tiledTerrainSystem.h"

using namespace game;
using namespace math;

void TiledTerrainSystem::ReleaseEntity(EntityID id)
{
	
}

void TiledTerrainSystem::Tick(const TickSystemContext& ctx, float dt)
{
	
}

void TiledTerrainSystem::GenerateWorld(const WorldGeneratorContext& ctx)
{
	m_width = ctx.m_width;
	m_height = ctx.m_height;
	
	const uint32 width = m_width;
	const uint32 height = m_height;
	
	const uint32 numTiles = width * height;
	
	m_positions.resize(numTiles);
	m_visualIndex.resize(numTiles);
	m_elevationAndOrientation.resize(numTiles);
	m_colors.resize(numTiles);
	
	for(uint32 h=0; h<height; ++h)
	{
		for(uint32 w=0; w<width; ++w)
		{
			const uint32 idx = h * width + w;
			
			m_positions[idx].x = width;
			m_positions[idx].y = height;
		}
	}
	
	for(uint32 i=0; i<numTiles; ++i)
	{
		m_visualIndex[i] = std::rand() % ctx.m_visualVariety;
	}
	
	for(uint32 i=0; i<numTiles; ++i)
	{
		const uint32 colorIdx = std::rand() % 2;
		const uint8 elevation = (colorIdx == 1) ? 0 : std::rand() % ctx.m_elevationVariety;
		const uint8 orientation = std::rand() % 4;
		
		m_colors[i] = colorIdx;
		m_elevationAndOrientation[i] = (elevation << 4) | orientation;
	}
}

void TiledTerrainSystem::CollectTileRendering(const CollectTileRenderingContext& ctx, CollectTileRenderingOutput& output)
{
	uint32 outIdx = 0;
	
	const float start_x = -(float)m_width / 2.0f;
	const float start_y = -(float)m_height / 2.0f;
	
	const float offset_xy = 0.5f;
	
	const double rotations[4] = {0_deg, 90_deg, 180_deg, 270_deg};
	
	const uint32 numMaxTilesVisible = (uint32)output.m_matrices.Size();
	
	const Vector3 colors[2] = { {0.1f, 0.8f, 0.1f}, {0.1f, 0.1f, 0.8f}};
	
	for(int32 h=0; h<m_height; ++h)
	{
		for(int32 w=0; w<m_width; ++w)
		{
			if(outIdx >= numMaxTilesVisible)
			{
				break;
			}
			
			const Vector2 distance = {(float)abs((int32)start_x + w - ctx.m_cameraPosX), (float)abs((int32)start_y + h - ctx.m_cameraPosY)};
			
			if(Mag(distance) < 15.0f)
			{
				const uint32 idx = h * m_width + w;
				
				const uint8 e_and_o = m_elevationAndOrientation[idx];
				uint8 elevation = e_and_o >> 4;
				uint8 orientation = e_and_o & 0x0F;
				
				math::Transform transform = math::CreateTransformIdentity();
				transform.translation = {(float)(start_x + w) + offset_xy, (float)(start_y + h) + offset_xy,
					((float)(elevation)/10.0f) * 0.05f - 0.1f, 0.0f};
				transform.rotation = math::CreateQuaternionRotationAxis(0.0f, 0.0f, 1.0f, rotations[orientation]);
				
				if(m_colors[idx] == 1)
				{
					transform.translation.z = -0.2f;
				}
				
				ConvertTransformToMatrix(transform, output.m_matrices[outIdx]);
				
				output.m_colors[outIdx] = colors[m_colors[idx]];
				
				++outIdx;
			}
		}
	}
	
	output.m_numTilesCollected = outIdx;
}
