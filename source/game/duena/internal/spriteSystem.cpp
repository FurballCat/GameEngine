#include "pch.h"
#include "spriteSystem.h"
#include "transformSystem.h"
#include "renderer/api.h"
#include "renderer/sprite.h"

using namespace game;
using namespace math;

void SpriteSystem::CreateSprite(EntityID id, const String& name, math::Vector2 offset, math::Vector2 scale, math::Vector3 color)
{
	FUR_ASSERT(m_spriteLibrary);
	
	rend::SpriteID spriteID = m_spriteLibrary->GetSpriteID(name);
	uint32 fps = m_spriteLibrary->GetSpriteFPS(spriteID);
	uint32 numFrames = m_spriteLibrary->NumFramesForSprite(spriteID);
	
	CreateSprite(id, spriteID, numFrames, fps, offset, scale, color);
}

void SpriteSystem::CreateSprite(EntityID id, rend::SpriteID spriteID, uint32 numFrames, uint32 fps, math::Vector2 offset, math::Vector2 scale, math::Vector3 color)
{
	uint32 index = m_numSprites;
	m_entityIDToIndex[id] = index;
	
	m_entityIDs[index] = id;
	m_spriteIDs[index] = spriteID;
	m_frameInterval[index] = 1.0f / (float)fps;
	m_timeToNextFrame[index] = m_frameInterval[index];
	m_numFrames[index] = numFrames;
	m_colors[index] = color;
	m_offsets[index] = offset;
	m_scales[index] = scale;
	m_currentFrames[index] = 0;
	
	++m_numSprites;
}

void SpriteSystem::ReleaseEntity(EntityID id)
{
	if(m_entityIDToIndex.find(id) == m_entityIDToIndex.end())
		return;
	
	uint32 index = m_entityIDToIndex[id];
	--m_numSprites;
	const uint32 lastIndex = m_numSprites;
	
	if(index != lastIndex)
	{
		m_entityIDs[index] = m_entityIDs[lastIndex];
		m_spriteIDs[index] = m_spriteIDs[lastIndex];
		m_frameInterval[index] = m_frameInterval[lastIndex];
		m_timeToNextFrame[index] = m_timeToNextFrame[lastIndex];
		m_numFrames[index] = m_numFrames[lastIndex];
		m_colors[index] = m_colors[lastIndex];
		m_offsets[index] = m_offsets[lastIndex];
		m_currentFrames[index] = m_currentFrames[lastIndex];
		
		m_entityIDToIndex[m_entityIDs[lastIndex]] = index;
	}
	
	m_entityIDToIndex.erase(id);
}

void SpriteSystem::Tick(const TickSystemContext& ctx, float dt)
{
	for(uint32 i=0; i<m_numSprites; ++i)
	{
		float timeLeft = m_timeToNextFrame[i];
		timeLeft -= dt;
		if(timeLeft < 0.0f)
		{
			timeLeft = m_frameInterval[i];
			m_currentFrames[i] = (m_currentFrames[i] + 1) % m_numFrames[i];
		}
		
		m_timeToNextFrame[i] = timeLeft;
	}
}

uint32 SpriteSystem::GetSpriteInstances(const GameSystemsProvider& systems, SpriteInstance* instances, uint32 numInstances, uint32 startIndex) const
{
	const TransformSystem* transformSystem = systems.GetSystem<TransformSystem>();
	
	FUR_ASSERT(numInstances <= m_numSprites);
	
	for(uint32 i=0; i<numInstances; ++i)
	{
		instances[i].m_color = m_colors[startIndex + i];
		instances[i].m_frame = m_spriteIDs[startIndex + i] + m_currentFrames[startIndex + i];
		instances[i].m_positionOffset = m_offsets[startIndex + i];
		math::ConvertTransformToMatrix(transformSystem->GetTransformFor(m_entityIDs[startIndex + i]), instances[i].m_modelToWorld);
		instances[i].m_modelToWorld.x *= m_scales[startIndex + i].x;
		instances[i].m_modelToWorld.y *= m_scales[startIndex + i].y;
	}
	
	return numInstances;
}
