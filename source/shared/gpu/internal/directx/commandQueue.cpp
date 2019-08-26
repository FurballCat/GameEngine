#include "pch.h"
#include "commandQueue.h"

using namespace gpu;

#ifdef PLATFORM_WINDOWS

CommandBuffer CommandQueue::CreateCommandBuffer()
{
    return CommandBuffer(nullptr);
}

#endif
