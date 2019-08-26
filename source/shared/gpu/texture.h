#pragma once

namespace gpu
{
    enum class PixelFormat;
    enum class TextureUsage;
	enum class StorageMode;
    
    struct GPU_API TextureDesc
    {
        uint32 m_width;
        uint32 m_height;
        PixelFormat m_format;
        TextureUsage m_usage;
		StorageMode m_storageMode = StorageMode::Managed;		// 'managed' is default for OS X (for iOS it's 'shared')
        bool m_mipmapped;
    };
	
	struct GPU_API Origin
	{
		uint32 x;
		uint32 y;
		uint32 z;
	};
	
	struct GPU_API Size
	{
		uint32 width;
		uint32 height;
		uint32 depth;
	};
	
	struct GPU_API Region
	{
		Origin m_origin;
		Size m_size;
	};
	
    class GPU_API Texture
    {
        DECLARE_GPU_PLATFORM_PTR(Texture, m_ptr);
    public:
		void ReplaceRegion(const Region& region, uint32 mipmapLevel, void* pixelBytes, uint32 bytesPerRow);
    };
}
