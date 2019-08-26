#pragma once

namespace gpu
{
    struct BlitCommandEncoderDesc
    {
        
    };
    
    class Texture;
    
    // Performs operations on textures and render targets like copy, fill etc.
    class GPU_API BlitCommandEncoder
    {
        DECLARE_GPU_PLATFORM_PTR(BlitCommandEncoder, m_ptr);
    public:
        // Copy from source texture into destination texture
        void Copy(const Texture& sourceTexture, uint32_t sourceSlice, uint32_t sourceLevel, const math::UintVector3& sourceOrigin,
                  const math::UintVector3& sourceSize,
                  const Texture& destinationTexture, uint32_t destinationSlice, uint32_t destinationLevel, const math::UintVector3& destinationOrigin);
        
        // Finish writing commands.
        void EndEncoding();
    };
}
