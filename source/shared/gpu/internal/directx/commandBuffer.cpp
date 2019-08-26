#include "pch.h"
#include "commandBuffer.h"
#include "graphicsPassDescriptor.h"
#include "graphicsCommandEncoder.h"
#include "blitCommandEncoder.h"

using namespace gpu;

#ifdef PLATFORM_WINDOWS

GraphicsCommandEncoder CommandBuffer::CreateGraphicsCommandEncoder(RenderPassDescriptor* desc)
{
    return GraphicsCommandEncoder(nullptr);
}

ParallelRenderCommandEncoder CommandBuffer::CreateParallelRenderCommandEncoder(RenderPassDescriptor* desc)
{
	return ParallelRenderCommandEncoder(nullptr);
}

BlitCommandEncoder CommandBuffer::CreateBlitCommandEncoder()
{
    return BlitCommandEncoder(nullptr);
}

void CommandBuffer::Present(Drawable* drawable)
{
    
}

void CommandBuffer::Enqueue()
{
    
}

void CommandBuffer::Commit()
{
    
}

void CommandBuffer::WaitUntilCompleted()
{
    
}

#endif
