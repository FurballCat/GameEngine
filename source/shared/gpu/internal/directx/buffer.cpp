#include "pch.h"
#include "buffer.h"

using namespace gpu;

#ifdef PLATFORM_WINDOWS

uint32 Buffer::GetSize() const
{
    return 0;
}

void* Buffer::GetContent()
{
    return nullptr;
}

#endif
