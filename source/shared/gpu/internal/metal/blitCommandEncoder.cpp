#include "pch.h"
#include "blitCommandEncoder.h"
#include "texture.h"

using namespace gpu;

#ifdef PLATFORM_OSX

void BlitCommandEncoder::Copy(const Texture& sourceTexture, uint32_t sourceSlice, uint32_t sourceLevel, const math::UintVector3& sourceOrigin,
                              const math::UintVector3& sourceSize,
                              const Texture& destinationTexture, uint32_t destinationSlice, uint32_t destinationLevel, const math::UintVector3& destinationOrigin)
{
    mtlpp::Origin sourceOriginMtl(sourceOrigin.x, sourceOrigin.y, sourceOrigin.z);
    mtlpp::Origin destinationOriginMtl(destinationOrigin.x, destinationOrigin.y, destinationOrigin.z);
    mtlpp::Size sourceSizeMtl(sourceSize.x, sourceSize.y, sourceSize.z);
    m_ptr.Copy(sourceTexture.GetPlatformPtr(), sourceSlice, sourceLevel, sourceOriginMtl, sourceSizeMtl, destinationTexture.GetPlatformPtr(),
               destinationSlice, destinationLevel, destinationOriginMtl);
}

void BlitCommandEncoder::EndEncoding()
{
    m_ptr.EndEncoding();
}

#endif
