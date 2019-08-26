#include "pch.h"
#include "shaderLibrary.h"
#include "shader.h"

using namespace gpu;

#ifdef PLATFORM_OSX

UniquePtr<Shader> ShaderLibrary::CreateShader(const char* entryFunctionName)
{
    Shader* shader = new Shader(m_ptr.NewFunction(entryFunctionName));
    return UniquePtr<Shader>(shader);
}

#endif
