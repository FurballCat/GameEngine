#pragma once

namespace gpu
{
	class Device;
	class Texture;
}

namespace rend
{
	class RENDERER_API Font
	{
	public:
		~Font();
		
		// gets path to two files with the same name (i.e. helvetica.font and helvetica.png, path="helvetica")
		static Font* LoadFont(const char* path, gpu::Device* device);
		
		static uint32 CalculateNumVerticesForText(uint32 numCharacters);
		static uint32 NumVerticesPerGlyph();
		
		const math::Vector4& GetGlyphFor(char chr) const;
		const gpu::Texture* GetAtlasTexture() const;
		
	private:
		HashMap<char, uint32> m_characterToGlyphIndexMapping;	// todo: refactor that, it can be done by sorting m_glyphs and using char as index
		math::Vector4* m_glyphs = nullptr;	// [x,y] position in atlas, [z,w] size
		uint32 m_numGlyphs = 0;
		UniquePtr<gpu::Texture> m_textureAtlas;
	};
	
	struct GlyphVertex
	{
		math::Vector2 m_position;
		math::Vector2 m_uv;
	};
	
	void FillTextVerticesFor(const Font* font, StringView text, ArrayView<GlyphVertex> vertices, float scale);
}
