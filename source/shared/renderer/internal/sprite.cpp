#include "pch.h"
#include "sprite.h"
#include "core/textParser.h"

using namespace rend;
using namespace math;

constexpr uint32 c_defaultSpriteFPS = 12;

namespace helper
{
	static UniquePtr<gpu::Texture> LoadTexture(const char* path, gpu::Device* device, uint32& outWidth, uint32& outHeight)
	{
		UniquePtr<gpu::Texture> texture;
		
		std::vector<uint8> textureData;
		outWidth = 0;
		outHeight = 0;
		
		if(lodepng::decode(textureData, outWidth, outHeight, path) != 0)
			return nullptr;
		
		{
			gpu::TextureDesc desc;
			desc.m_width = outWidth;
			desc.m_height = outHeight;
			desc.m_format = gpu::PixelFormat::RGBA8Unorm;
			desc.m_mipmapped = false;
			desc.m_storageMode = gpu::StorageMode::Managed;
			desc.m_usage = gpu::TextureUsage::ShaderRead;
			texture = device->CreateTexture(desc);
			
			gpu::Region region {{0, 0, 0}, {outWidth, outHeight, 1}};
			texture->ReplaceRegion(region, 0, textureData.data(), outWidth * 4);
		}
		
		return texture;
	}
}

SpriteLibrary* SpriteLibrary::LoadSpriteLibrary(const char* path, gpu::Device* device)
{
	SpriteLibrary* spriteLib = new SpriteLibrary();
	
	// load atlas texture
	String pathTextureAtlas(path);
	pathTextureAtlas += ".png";
	uint32 width = 0;
	uint32 height = 0;
	spriteLib->m_atlasTexture = helper::LoadTexture(pathTextureAtlas.c_str(), device, width, height);
	FUR_ASSERT(spriteLib->m_atlasTexture);
	
	// load sprite lib metadata
	String pathSpriteLib(path);
	pathSpriteLib += ".spritelib";
	FileStream file(pathSpriteLib, FileStream::in);
	FUR_ASSERT(file);
	TextParser parser(file);
	
	// get number of sprites
	DynArray<IntVector4> frames;
	DynArray<String> spriteNames;
	DynArray<SpriteID> spriteIDs;
	DynArray<uint32> spriteFPSs;
	
	while(parser.ParseKeyword("s") && parser.IsParsing())
	{
		// get sprite name
		String name;
		bool result = parser.ParseText(name);
		FUR_ASSERT(result);
		
		spriteNames.push_back(name);
		
		// get sprite frames per second
		int32 fps = (int32)c_defaultSpriteFPS;
		if(parser.ParseKeyword("fps"))
		{
			parser.ParseInt(fps);
		}
		spriteFPSs.push_back((uint32)fps);
		
		spriteLib->m_spriteNameToSpriteIDsIndex[name] = (uint32)spriteIDs.size();
		spriteIDs.push_back((uint32)frames.size());
		
		// get frames
		while(parser.ParseKeyword("f") && parser.IsParsing())
		{
			IntVector4 frame;
			result = parser.ParseInt4(frame.x, frame.y, frame.z, frame.w);
			FUR_ASSERT(result);
			
			frames.push_back(frame);
		}
	}
	
	uint32 numAllFrames = (uint32)frames.size();
	
	FUR_ASSERT(spriteIDs.size() == spriteNames.size());
	
	spriteLib->m_numSprites = (uint32)spriteNames.size();
	spriteLib->m_numFrames = numAllFrames;
	spriteLib->m_spriteIDs = new SpriteID[spriteLib->m_numSprites];
	spriteLib->m_frames = new IntVector4[numAllFrames];
	spriteLib->m_spriteFPSs = new uint32[spriteLib->m_numSprites];
	
	std::memcpy(spriteLib->m_spriteIDs, spriteIDs.data(), spriteIDs.size() * sizeof(SpriteID));
	std::memcpy(spriteLib->m_frames, frames.data(), frames.size() * sizeof(IntVector4));
	std::memcpy(spriteLib->m_spriteFPSs, spriteFPSs.data(), spriteFPSs.size() * sizeof(uint32));
	
	DynArray<Vector4> framesUVs;
	framesUVs.resize(numAllFrames);
	for(uint32 i=0; i<numAllFrames; ++i)
	{
		const IntVector4& f = spriteLib->m_frames[i];
		framesUVs[i] = {f.x / (float)width, f.y / (float)height, f.z / (float)width, f.w / (float)height};
	}
	
	// create UVs buffer
	{
		gpu::BufferDesc desc;
		desc.m_data = framesUVs.data();
		desc.m_size = sizeof(Vector4) * numAllFrames;
		desc.m_options = gpu::ResourceOptions::StorageModePrivate;	// only accessible by GPU
		spriteLib->m_framesUVs = device->CreateBuffer(desc);
	}
	
	return spriteLib;
}

SpriteLibrary::~SpriteLibrary()
{
	m_atlasTexture.reset();
	m_framesUVs.reset();
	
	if(m_frames)
		delete [] m_frames;
	
	if(m_spriteIDs)
		delete [] m_spriteIDs;
	
	if(m_spriteFPSs)
		delete [] m_spriteFPSs;
	
	m_numFrames = 0;
	m_numSprites = 0;
	
	m_spriteNameToSpriteIDsIndex.clear();
}

SpriteID SpriteLibrary::GetSpriteID(const String& spriteName) const
{
	auto it = m_spriteNameToSpriteIDsIndex.find(spriteName);
	if(it != m_spriteNameToSpriteIDsIndex.end())
	{
		return GetSpriteIDByIndex(it->second);
	}
	
	return INVALID_SPRITE_ID;
}

uint32 SpriteLibrary::GetSpriteFPS(SpriteID id) const
{
	FUR_ASSERT(IsValidSpriteID(id));
	
	// todo: can try lower_bound on this to speed up search, probably depends on number of sprites in a library
	for(uint32 i=0; i<m_numSprites; ++i)
	{
		if(m_spriteIDs[i] == id)
		{
			return m_spriteFPSs[i];
		}
	}
	
	return c_defaultSpriteFPS;
}

uint32 SpriteLibrary::NumFramesForSprite(SpriteID id) const
{
	FUR_ASSERT(IsValidSpriteID(id));
	
	// todo: can try lower_bound on this to speed up search, probably depends on number of sprites in a library
	for(uint32 i=0; i<m_numSprites; ++i)
	{
		if(m_spriteIDs[i] == id)
		{
			if(i < m_numSprites - 1)
				return m_spriteIDs[i+1] - id;
			else
				return m_numFrames - id;
		}
	}
	
	return 0;
}

uint32 SpriteLibrary::GetFramesForSprite(SpriteID id, IntVector4* outFrames, uint32 numFrames, uint32 startIndex) const
{
	FUR_ASSERT(IsValidSpriteID(id));
	FUR_ASSERT(id + startIndex + numFrames <= m_numFrames); // note: sprite id is an index of first frame for given sprite in all frames the sprite library
	FUR_ASSERT(numFrames <= NumFramesForSprite(id));
	
	uint32 startFrame = id + startIndex;
	for(uint32 i=0; i<numFrames; ++i)
		outFrames[i] = m_frames[startFrame + i];
	
	return numFrames;
}

