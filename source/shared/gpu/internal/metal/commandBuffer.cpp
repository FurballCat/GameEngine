#include "pch.h"
#include "commandBuffer.h"
#include "graphicsPassDescriptor.h"
#include "graphicsCommandEncoder.h"
#include "blitCommandEncoder.h"
#include "drawable.h"

using namespace gpu;

#ifdef PLATFORM_OSX

GraphicsCommandEncoder CommandBuffer::CreateGraphicsCommandEncoder(RenderPassDescriptor* desc)
{
    auto descPtr = desc->GetPlatformPtr();
    return GraphicsCommandEncoder(m_ptr.RenderCommandEncoder(descPtr));
}

ParallelRenderCommandEncoder CommandBuffer::CreateParallelRenderCommandEncoder(RenderPassDescriptor* desc)
{
	auto descPtr = desc->GetPlatformPtr();
	return ParallelRenderCommandEncoder(m_ptr.ParallelRenderCommandEncoder(descPtr));
}

BlitCommandEncoder CommandBuffer::CreateBlitCommandEncoder()
{
    return BlitCommandEncoder(m_ptr.BlitCommandEncoder());
}

void CommandBuffer::Present(Drawable* drawable)
{
    m_ptr.Present(drawable->GetPlatformPtr());
}

void CommandBuffer::Enqueue()
{
    m_ptr.Enqueue();
}

void CommandBuffer::Commit()
{
    m_ptr.Commit();
}

void CommandBuffer::WaitUntilCompleted()
{
    m_ptr.WaitUntilCompleted();
}

#endif
