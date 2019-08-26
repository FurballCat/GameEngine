#pragma once

namespace gpu
{
    struct GraphicsCommandEncoderDesc
    {
        
    };
    
    enum class PrimitiveType;
    class GraphicsPipelineState;
    class Buffer;
	class DepthStencilState;
	class Texture;
    
    // Lightweight (transient), non-thread-safe (unless parallel encoder is used), used for writing commands to CommandBuffer.
    // It encodes commands in a format that the GPU can execute. While active it has exclusive access to CommandBuffer.
    // Once you finish writing commands, call EndEncoding. To write further commands, create a new command encoder.
    // One render command encoder represents a single render pass, so it's associated with RenderPassDescriptor, that includes
    // render targets (color, depth and stencil attachments that serve as destinations for rendering commands).
    // todo: rename to RenderCommandEncoder
    class GPU_API GraphicsCommandEncoder
    {
        DECLARE_GPU_PLATFORM_PTR(GraphicsCommandEncoder, m_ptr);
    public:
        // Set compiled rendering state, including vertex and pixel shaders.
        void SetRenderPipelineState(GraphicsPipelineState* state);
        
        // Set buffer for vertex shader (any buffer).
        void SetVertexBuffer(const Buffer* buffer, uint32 offset, uint32 index);
        
        // Set buffer for pixel shader (any buffer).
        void SetPixelBuffer(const Buffer* buffer, uint32 offset, uint32 index);
		
		// Set texture for fragment (pixel) shader
		void SetFragmentTexture(const Texture* texture, uint32 index);
		
		// Set depth stencil state for rasterizer
		void SetDepthStencilState(DepthStencilState* state);
        
        // Draw 3D primitive.
        void Draw(PrimitiveType primitiveType, uint32 startIndex, uint32 vertexCount);
		void Draw(PrimitiveType primitiveType, uint32 startIndex, uint32 vertexCount, uint32 instanceCount);
        
        // todo: add viewport, triangle fill mode, scissor rectangle, depth and stencil tests and other value setters
        
        // Finish writing commands.
        void EndEncoding();
    };
	
	// Lightweight (transient), thread-safe, the rest is the same as in regular RenderCommandEncoder
	class GPU_API ParallelRenderCommandEncoder
	{
		DECLARE_GPU_PLATFORM_PTR(ParallelRenderCommandEncoder, m_ptr);
	public:
		// you can create multiple render command encoders out of it, yeach can be used on separate thread
		GraphicsCommandEncoder CreateRenderCommandEncoder();
		
		// call EndEncoding on yeach command encoder created by this encoder, then call parallel EndEncoding
		void EndEncoding();
	};
}
