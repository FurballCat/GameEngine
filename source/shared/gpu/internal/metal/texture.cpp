#include "pch.h"
#include "texture.h"

using namespace gpu;

#ifdef PLATFORM_OSX

void Texture::ReplaceRegion(const Region& region, uint32 mipmapLevel, void* pixelBytes, uint32 bytesPerRow)
{
	mtlpp::Region mtlppRegion(region.m_origin.x, region.m_origin.y, region.m_origin.z,
							  region.m_size.width, region.m_size.height, region.m_size.depth);
	
	m_ptr.Replace(mtlppRegion, mipmapLevel, pixelBytes, bytesPerRow);
}

#endif
