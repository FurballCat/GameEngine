#pragma once

namespace gpu
{
	class Device;
	class Texture;
	class Buffer;
}

namespace rend
{
	// implementation note: internally sprite id is just an index of first frame in the frames array for given sprite
	typedef uint32 SpriteID;
	constexpr SpriteID INVALID_SPRITE_ID = 0xFFFFFFFF;
	
	class RENDERER_API SpriteLibrary
	{
	public:
		// actually loads two files, so the path shouldn't contain file extension (so for example path should be "assets/sprite_lib_a")
		// then it will load "assets/sprite_lib_a.spritelib" and "assets/sprite_lib_a.png"
		static SpriteLibrary* LoadSpriteLibrary(const char* path, gpu::Device* device);
		
		~SpriteLibrary();
		
		SpriteID GetSpriteID(const String& spriteName) const;
		
		SpriteID GetSpriteIDByIndex(uint32 index) const { FUR_ASSERT(index < m_numSprites); return m_spriteIDs[index]; }
		uint32 NumSprites() const { return m_numSprites; }
		uint32 GetSpriteFPS(SpriteID id) const;
		
		uint32 NumFramesForSprite(SpriteID id) const;
		uint32 GetFramesForSprite(SpriteID id, math::IntVector4* outFrames, uint32 numFrames, uint32 startIndex = 0) const;
		
		const gpu::Texture* GetAtlasTexture() const { return m_atlasTexture.get(); }
		const gpu::Buffer* GetFramesUVs() const { return m_framesUVs.get(); }
		
	private:
		// sprite id is valid only if it's lower than the number of all frames in sprite library
		bool IsValidSpriteID(SpriteID id) const { return id < m_numFrames; }
		
		// atlas texture containing all the sprites
		UniquePtr<gpu::Texture> m_atlasTexture;
		
		// one frame is Vector4: [x,y] position in texture, [z,w] width and height of the frame in texture, all lengths are normalized to 0..1
		UniquePtr<gpu::Buffer> m_framesUVs;
		
		// all frames in pixel sizes, kept for quetires, for example when creating collisions
		math::IntVector4* m_frames = nullptr;
		
		// array of sprite IDs
		SpriteID* m_spriteIDs = nullptr;
		
		// frames per second for each sprite
		uint32* m_spriteFPSs = nullptr;
		
		// number of all frames in all sprites
		uint32 m_numFrames = 0;
		
		// number of sprites
		uint32 m_numSprites = 0;
		
		// todo: rethink that
		HashMap<String, uint32> m_spriteNameToSpriteIDsIndex;
	};
}
