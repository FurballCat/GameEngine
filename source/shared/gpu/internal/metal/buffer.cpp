#include "pch.h"
#include "buffer.h"

using namespace gpu;

#ifdef PLATFORM_OSX

uint32 Buffer::GetSize() const
{
    return m_ptr.GetLength();
}

void* Buffer::GetContent()
{
    return m_ptr.GetContents();
}

#endif
