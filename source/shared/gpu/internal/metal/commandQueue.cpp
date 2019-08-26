#include "pch.h"
#include "commandQueue.h"

using namespace gpu;

#ifdef PLATFORM_OSX

CommandBuffer CommandQueue::CreateCommandBuffer()
{
    return CommandBuffer(m_ptr.CommandBuffer());
}

#endif
