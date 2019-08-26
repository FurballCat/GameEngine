#include "pch.h"
#include "shaderLibrary.h"
#include "shader.h"

using namespace gpu;

#ifdef PLATFORM_WINDOWS

UniquePtr<Shader> ShaderLibrary::CreateShader(const char* entryFunctionName)
{
    return nullptr;
}

#endif
