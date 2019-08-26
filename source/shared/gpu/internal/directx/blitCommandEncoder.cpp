#include "pch.h"
#include "blitCommandEncoder.h"

using namespace gpu;

#ifdef PLATFORM_WINDOWS

void BlitCommandEncoder::Copy(const Texture& sourceTexture, uint32_t sourceSlice, uint32_t sourceLevel, const math::UintVector3& sourceOrigin,
          const math::UintVector3& sourceSize,
          const Texture& destinationTexture, uint32_t destinationSlice, uint32_t destinationLevel, const math::UintVector3& destinationOrigin)
{
    
}

void BlitCommandEncoder::EndEncoding()
{
    
}

#endif
