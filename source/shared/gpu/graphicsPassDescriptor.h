#pragma once

namespace gpu
{
    class Texture;
	class VertexDescriptor;
    
    constexpr uint32 c_maxColorAttachments = 4;     // There's up to 4 color attachments (render targets for color) in Metal
    
    struct ClearColor
    {
        float r, g, b, a;
    };
    
    struct ColorAttachmentDesc
    {
		// render target texture buffer (where to draw to)
        Texture* m_texture = nullptr;
		
		// load action is performed at the begining of render pass, this can clear the buffer, or load previous pass' results or do nothing
        LoadAction m_loadAction;
		
		// store action is performed at the end of render pass, this can store output in render target, or do some other things
        StoreAction m_storeAction;
		
		// clear color that is used to fill render target in case of clear load action
        ClearColor m_clearColor;
    };
	
	struct DepthAttachmentDesc
	{
		// render target texture buffer (where to draw to)
		Texture* m_texture = nullptr;
		
		// load action is performed at the begining of render pass, this can clear the buffer, or load previous pass' results or do nothing
		LoadAction m_loadAction;
		
		// store action is performed at the end of render pass, this can store output in render target, or do some other things
		StoreAction m_storeAction;
		
		// clear value that is used to fill render target in case of clear load action
		double m_clearDepth;
	};
	
    struct RenderPassDescriptorDesc
    {
		// render targets used in this render pass
        ColorAttachmentDesc m_colorAttachments[c_maxColorAttachments];
		
		// depth render target
		DepthAttachmentDesc m_depthAttachment;
		
		// vertex layout and vertex buffer layouts. I think this also mean that you can use multiple vertex buffers, and fetch data from different buffers.
		VertexDescriptor* m_vertexDescription = nullptr;
    };
    
    class GPU_API RenderPassDescriptor
    {
        DECLARE_GPU_PLATFORM_PTR(RenderPassDescriptor, m_ptr);
    public:
        RenderPassDescriptor();
		
		// get render target from slot with given index
		Texture GetColorAttachmentTexture(uint32 index);
        
        static UniquePtr<RenderPassDescriptor> Create(RenderPassDescriptorDesc& desc);
    };
}
