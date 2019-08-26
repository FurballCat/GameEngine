#include "pch.h"
#include "graphicsCommandEncoder.h"

using namespace gpu;

#ifdef PLATFORM_WINDOWS

void GraphicsCommandEncoder::SetRenderPipelineState(GraphicsPipelineState* state)
{
    
}

void GraphicsCommandEncoder::SetVertexBuffer(const Buffer* buffer, uint32 offset, uint32 index)
{
    
}

void GraphicsCommandEncoder::SetPixelBuffer(const Buffer* buffer, uint32 offset, uint32 index)
{
    
}

void GraphicsCommandEncoder::SetFragmentTexture(const Texture* texture, uint32 index)
{
	
}

void GraphicsCommandEncoder::SetDepthStencilState(DepthStencilState* state)
{
	
}

void GraphicsCommandEncoder::Draw(PrimitiveType primitiveType, uint32 startIndex, uint32 vertexCount)
{
    
}

void GraphicsCommandEncoder::Draw(PrimitiveType primitiveType, uint32 startIndex, uint32 vertexCount, uint32 instanceCount)
{
	
}

void GraphicsCommandEncoder::EndEncoding()
{
    
}

//---

GraphicsCommandEncoder ParallelRenderCommandEncoder::CreateRenderCommandEncoder()
{
	return GraphicsCommandEncoder(nullptr);
}

void ParallelRenderCommandEncoder::EndEncoding()
{

}


#endif
