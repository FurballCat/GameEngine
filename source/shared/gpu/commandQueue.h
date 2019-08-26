#pragma once
#include "commandBuffer.h"

namespace gpu
{
    struct GPU_API CommandQueueDesc
    {
        
    };
    
    class CommandBuffer;
    
    // Heavy to create, thread-safe, expected to be long-lived, created by Device.
    // Accepts ordered list of command buffers. Command buffers are executed in order in which they were encoded.
    // Is thread-safe and accepts many command buffers to be encoded simultaneously.
    // Is expected to be long-lived, so they should not be repeatedly created and destroyed.
    class GPU_API CommandQueue
    {
        DECLARE_GPU_PLATFORM_PTR(CommandQueue, m_ptr);
    public:
        
        // Create lightweight buffer for commands to be encoded.
        CommandBuffer CreateCommandBuffer();
    };
}
