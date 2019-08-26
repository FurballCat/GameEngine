#include "pch.h"
#include "cameraSystem.h"
#include "transformSystem.h"

using namespace game;
using namespace math;

static float g_distance = 25.0f;

void CameraSystem::AddEntityToFollow(EntityID id)
{
	FUR_ASSERT(m_numEntitiesFollowed < NUM_MAX_ENTITIES_TO_FOLLOW);
	
	uint32 index = m_numEntitiesFollowed;
	
	m_entityToFollowIDs[index] = id;
	
	++m_numEntitiesFollowed;
}

void CameraSystem::ReleaseEntity(EntityID id)
{
	int32 index = -1;
	for(int32 i=0; i<m_numEntitiesFollowed; ++i)
	{
		if(m_entityToFollowIDs[i] == id)
		{
			index = i;
			break;
		}
	}
	
	if(index == -1)
		return;
	
	--m_numEntitiesFollowed;
	uint32 lastIndex = m_numEntitiesFollowed;
	
	if(index != lastIndex)
	{
		m_entityToFollowIDs[index] = m_entityToFollowIDs[lastIndex];
	}
}

void CameraSystem::Initialize(const InitializeSystemContext& ctx)
{
	m_eye = {g_distance, g_distance, g_distance * 1.41f * Cos((60.0f / 180.0f) * math::c_pi)};
}

void CameraSystem::Tick(const game::TickSystemContext &ctx, float dt)
{
	if(m_numEntitiesFollowed > 0)
	{
		TransformSystem* sysTransform = ctx.GetSystem<TransformSystem>();
		
		Vector4 avgPosition = {0.0f, 0.0f, 0.0f, 0.0f};
		for(uint32 i=0; i<m_numEntitiesFollowed; ++i)
		{
			avgPosition += sysTransform->GetTransformFor(m_entityToFollowIDs[i]).translation;
		}
		
		avgPosition /= (float)m_numEntitiesFollowed;
		
		Vector3 eye = {g_distance, g_distance, g_distance * 1.41f * Cos((60.0f / 180.0f) * math::c_pi)};
		
		m_at = math::GetXYZ(avgPosition);
		m_eye = eye + m_at;
	}
}
