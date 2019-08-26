#include "pch.h"
#include "graphicsCommandEncoder.h"
#include "graphicsPipelineState.h"
#include "depthStencilState.h"
#include "buffer.h"
#include "enums.h"
#include "texture.h"

using namespace gpu;

#ifdef PLATFORM_OSX

void GraphicsCommandEncoder::SetRenderPipelineState(GraphicsPipelineState* state)
{
    m_ptr.SetRenderPipelineState(state->GetPlatformPtr());
}

void GraphicsCommandEncoder::SetVertexBuffer(const Buffer* buffer, uint32 offset, uint32 index)
{
    m_ptr.SetVertexBuffer(buffer->GetPlatformPtr(), offset, index);
}

void GraphicsCommandEncoder::SetPixelBuffer(const Buffer* buffer, uint32 offset, uint32 index)
{
    m_ptr.SetFragmentBuffer(buffer->GetPlatformPtr(), offset, index);
}

void GraphicsCommandEncoder::SetFragmentTexture(const Texture* texture, uint32 index)
{
	m_ptr.SetFragmentTexture(texture->GetPlatformPtr(), index);
}

void GraphicsCommandEncoder::SetDepthStencilState(DepthStencilState* state)
{
	m_ptr.SetDepthStencilState(state->GetPlatformPtr());
}

void GraphicsCommandEncoder::Draw(PrimitiveType primitiveType, uint32 startIndex, uint32 vertexCount)
{
    m_ptr.Draw(ToNativeEnum(primitiveType), startIndex, vertexCount);
}

void GraphicsCommandEncoder::Draw(PrimitiveType primitiveType, uint32 startIndex, uint32 vertexCount, uint32 instanceCount)
{
	m_ptr.Draw(ToNativeEnum(primitiveType), startIndex, vertexCount, instanceCount);
}

void GraphicsCommandEncoder::EndEncoding()
{
    m_ptr.EndEncoding();
}

//---

GraphicsCommandEncoder ParallelRenderCommandEncoder::CreateRenderCommandEncoder()
{
	return GraphicsCommandEncoder(m_ptr.GetRenderCommandEncoder());
}

void ParallelRenderCommandEncoder::EndEncoding()
{
	m_ptr.EndEncoding();
}

#endif
