#pragma once

namespace rend
{
	typedef uint32 SpriteID;
	class SpriteLibrary;
}

namespace game
{
	struct SpriteInstance
	{
		math::Matrix4x4 m_modelToWorld;
		math::Vector3 m_color;
		uint32 m_frame;
		math::Vector2 m_positionOffset;
	};
	
	// test code
	class SpriteSystem : public IGameSystem
	{
	public:
		void Initialize(const rend::SpriteLibrary* spriteLibrary) { m_spriteLibrary = spriteLibrary; }
		
		void CreateSprite(EntityID id, const String& name, math::Vector2 offset, math::Vector2 scale, math::Vector3 color);
		void CreateSprite(EntityID id, rend::SpriteID spriteID, uint32 numFrames, uint32 fps, math::Vector2 offset, math::Vector2 scale, math::Vector3 color);
		
		virtual void ReleaseEntity(EntityID id) override;
		virtual void Tick(const TickSystemContext& ctx, float dt) override;
		uint32 GetSpriteInstances(const GameSystemsProvider& systems, SpriteInstance* instances, uint32 numInstances, uint32 startIndex = 0) const;
		
		uint32 NumSprites() const { return m_numSprites; }
		
	private:
		static const uint32 NUM_MAX_SPRITES = 2048;
		
		HashMap<EntityID, uint32> m_entityIDToIndex;
		EntityID m_entityIDs[NUM_MAX_SPRITES];
		rend::SpriteID m_spriteIDs[NUM_MAX_SPRITES];
		math::Vector3 m_colors[NUM_MAX_SPRITES];
		math::Vector2 m_offsets[NUM_MAX_SPRITES];
		math::Vector2 m_scales[NUM_MAX_SPRITES];
		uint32 m_currentFrames[NUM_MAX_SPRITES];
		uint32 m_numFrames[NUM_MAX_SPRITES];
		float m_timeToNextFrame[NUM_MAX_SPRITES];
		float m_frameInterval[NUM_MAX_SPRITES];
		
		uint32 m_numSprites = 0;
		
		const rend::SpriteLibrary* m_spriteLibrary = nullptr;
	};
}
