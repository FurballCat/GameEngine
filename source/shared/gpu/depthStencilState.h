#pragma once

namespace gpu
{
    class Shader;
	
    struct GPU_API DepthStencilStateDesc
    {
		CompareFunction m_compareFunction;
		bool m_isDepthWriteEnabled;
    };
    
    class GPU_API DepthStencilState
    {
        DECLARE_GPU_PLATFORM_PTR(DepthStencilState, m_ptr);
    public:
    };
}
