#include "pch.h"
#include "font.h"
#include "core/textParser.h"

using namespace rend;
using namespace math;

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

Font::~Font()
{
	if(m_glyphs)
		delete [] m_glyphs;
	
	m_numGlyphs = 0;
	
	m_textureAtlas.reset();
}

Font* Font::LoadFont(const char *path, gpu::Device *device)
{
	Font* font = new Font();
	
	String pathTextureAtlas(path);
	pathTextureAtlas += ".png";
	uint32 width = 0;
	uint32 height = 0;
	font->m_textureAtlas = helper::LoadTexture(pathTextureAtlas.c_str(), device, width, height);
	
	FUR_ASSERT(font->m_textureAtlas);
	
	String pathGlyphs(path);
	pathGlyphs += ".font";
	FileStream fileGlyphs(pathGlyphs, FileStream::in);
	
	FUR_ASSERT(fileGlyphs);
	
	DynArray<char> characters;
	DynArray<IntVector4> glyphs;
	
	TextParser parser(fileGlyphs);
	while(parser.IsParsing())
	{
		char chr;
		if(parser.ParseNonWhitespaceCharacter(chr))
		{
			characters.push_back(chr);
			font->m_characterToGlyphIndexMapping[chr] = (uint32)glyphs.size();
		}
		else
		{
			break;
		}
		
		IntVector4 glyph = {0, 0, 0, 0};
		if(parser.ParseInt4(glyph.x, glyph.y, glyph.z, glyph.w))
		{
			glyphs.push_back(glyph);
		}
		else
		{
			break;
		}
	}
	
	font->m_glyphs = new Vector4[glyphs.size()];
	
	for(uint32 i=0; i<glyphs.size(); ++i)
	{
		font->m_glyphs[i].x = glyphs[i].x / (float)width;
		font->m_glyphs[i].y = (height - glyphs[i].y - glyphs[i].w) / (float)height;
		font->m_glyphs[i].z = glyphs[i].z / (float)width;
		font->m_glyphs[i].w = glyphs[i].w / (float)height;
	}
	
	font->m_numGlyphs = (uint32)glyphs.size();
	
	return font;
}

uint32 Font::NumVerticesPerGlyph()
{
	return 6;	// two triangles (3 + 3 vertices)
}

uint32 Font::CalculateNumVerticesForText(uint32 numCharacters)
{
	return numCharacters * NumVerticesPerGlyph();
}

const math::Vector4& Font::GetGlyphFor(char chr) const
{
	auto it = m_characterToGlyphIndexMapping.find(chr);
	FUR_ASSERT(it != m_characterToGlyphIndexMapping.end());
	return m_glyphs[it->second];
}

const gpu::Texture* Font::GetAtlasTexture() const
{
	return m_textureAtlas.get();
}

void rend::FillTextVerticesFor(const Font* font, StringView text, ArrayView<GlyphVertex> vertices, float scale)
{
	uint32 length = (uint32)text.Size();
	
	FUR_ASSERT(vertices.Size() >= Font::CalculateNumVerticesForText(length));
	FUR_ASSERT(font);
	
	Vector2 cursor = {0.0f, 0.0f};
	
	for(uint32 i=0; i<length; ++i)
	{
		const Vector4 glyph = font->GetGlyphFor(text[i]);
		const Vector4 scaledGlyph = glyph * scale;
		
		const float glyphWidth = glyph.z;
		const float glyphHeight = glyph.w;
		
		const float scaledGlyphWidth = scaledGlyph.z;
		const float scaledGlyphHeight = scaledGlyph.w;
		
		const uint32 vID = i * Font::NumVerticesPerGlyph();
		
		// top-right CCW triangle
		// O---
		//  \  |
		//   \ |
		//    \|
		//
		vertices[vID + 0].m_position = cursor;
		vertices[vID + 0].m_uv = {glyph.x, glyph.y};
		
		//  ---
		//  \  |
		//   \ |
		//    \|
		//     O
		
		vertices[vID + 1].m_position = {cursor.x + scaledGlyphWidth, cursor.y - scaledGlyphHeight};
		vertices[vID + 1].m_uv = {glyph.x + glyphWidth, glyph.y + glyphHeight};
		
		//  ---O
		//  \  |
		//   \ |
		//    \|
		//
		
		vertices[vID + 2].m_position = {cursor.x + scaledGlyphWidth, cursor.y};
		vertices[vID + 2].m_uv = {glyph.x + glyphWidth, glyph.y};
		
		// bottom-left CCW triangle
		// O
		// |\
		// | \
		// |  \
		//  ---
		vertices[vID + 3] = vertices[vID + 0];
		
		//
		// |\
		// | \
		// |  \
		// O---
		vertices[vID + 4].m_position = {cursor.x, cursor.y - scaledGlyphHeight};
		vertices[vID + 4].m_uv = {glyph.x, glyph.y + glyphHeight};
		
		//
		// |\
		// | \
		// |  \
		//  ---O
		vertices[vID + 5] = vertices[vID + 1];
		
		cursor.x += scaledGlyphWidth;
	}
}























