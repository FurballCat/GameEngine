#pragma once

namespace gpu
{
    class Shader;
    
    struct GPU_API CompileShadersOptions
    {
        
    };
    
    struct GPU_API CompileShadersErrors
    {
        
    };
    
    class GPU_API ShaderLibrary
    {
        DECLARE_GPU_PLATFORM_PTR(ShaderLibrary, m_ptr);
    public:
        UniquePtr<Shader> CreateShader(const char* entryFunctionName);
    };
}
