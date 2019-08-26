#pragma once

namespace gpu
{
    class Shader;
	
	struct GPU_API GraphicsPipelineStateColorAttachmentDesc
	{
		PixelFormat m_pixelFormat;
	};
	
    struct GPU_API GraphicsPipelineStateDesc
    {
        Shader* m_vertexShader = nullptr;
        Shader* m_pixelShader = nullptr;
		
		GraphicsPipelineStateColorAttachmentDesc* m_colorAttachments = nullptr;
		uint32 m_numColorAttachments = 0;
    };
    
    class GPU_API GraphicsPipelineState
    {
        DECLARE_GPU_PLATFORM_PTR(GraphicsPipelineState, m_ptr);
    public:
    };
}
