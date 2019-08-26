#include "pch.h"
#include "debugMeshRenderSystem.h"
#include "transformSystem.h"

using namespace game;
using namespace math;

void game::DebugMeshRenderSystem::VisualizeEntity(EntityID id, float scale, math::Vector3 color)
{
	FUR_ASSERT(m_numMeshes < NUM_MAX_MESHES);
	
	uint32 i = m_numMeshes;
	++m_numMeshes;
	
	m_scales[i] = scale;
	m_entityIDs[i] = id;
	m_colors[i] = color;
}

void game::DebugMeshRenderSystem::ReleaseEntity(EntityID id)
{
	constexpr uint32 invalidIndex = 0xFFFFFFFF;
	uint32 removeIndex = invalidIndex;
	for(uint32 i=0; i<m_numMeshes; ++i)
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
	FUR_ASSERT(m_numMeshes > 0);
	
	--m_numMeshes;
	uint32 lastIndex = m_numMeshes;
	
	if(lastIndex != removeIndex)
	{
		m_entityIDs[removeIndex] = m_entityIDs[lastIndex];
		m_scales[removeIndex] = m_scales[lastIndex];
		m_colors[removeIndex] = m_colors[lastIndex];
	}
}

uint32 game::DebugMeshRenderSystem::CalculateMatrices(GameSystemsProvider systems, math::Matrix4x4* matrices, uint32 numMatrices, uint32 startIndex) const
{
	TransformSystem* sysTransform = systems.GetSystem<TransformSystem>();
	
	int32 count = Clamp((int32)m_numMeshes - (int32)startIndex, 0, numMatrices);
	for(int32 i=0; i<count; ++i)
	{
		EntityID id = m_entityIDs[startIndex + i];
		const Transform& transform = sysTransform->GetTransformFor(id);
		math::ConvertTransformToMatrix(transform, matrices[i]);
		
		float scale = m_scales[startIndex + i];
		matrices[i].x *= scale;
		matrices[i].y *= scale;
		matrices[i].z *= scale;
	}
	
	return count;
}

uint32 game::DebugMeshRenderSystem::GetColors(math::Vector3* colors, uint32 numColors, uint32 startIndex) const
{
	int32 count = Clamp((int32)m_numMeshes - (int32)startIndex, 0, numColors);
	for(int32 i=0; i<count; ++i)
	{
		colors[i] = m_colors[i];
	}
	
	return count;
}
