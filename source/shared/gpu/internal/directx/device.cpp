#include "pch.h"
#include "device.h"
#include "buffer.h"
#include "shaderLibrary.h"
#include "depthStencilState.h"
#include "texture.h"
#include "graphicsPipelineState.h"
#include "commandQueue.h"

using namespace gpu;

#ifdef PLATFORM_WINDOWS

UniquePtr<Device> Device::CreateDefault()
{
    return nullptr;
}

UniquePtr<Buffer> Device::CreateBuffer(const gpu::BufferDesc &desc)
{
    return nullptr;
}

UniquePtr<CommandQueue> gpu::Device::CreateCommandQueue(const CommandQueueDesc & desc)
{
	return nullptr;
}

UniquePtr<CommandQueue> CreateCommandQueue(const CommandQueueDesc& desc)
{
    return nullptr;
}

UniquePtr<GraphicsPipelineState> CreateGraphicsPipelineState(const GraphicsPipelineStateDesc& desc)
{
    return nullptr;
}

UniquePtr<GraphicsPipelineState> gpu::Device::CreateGraphicsPipelineState(const GraphicsPipelineStateDesc & desc)
{
	return nullptr;
}

UniquePtr<ShaderLibrary> Device::CompileShaders(const char* sourceCode, const CompileShadersOptions& options, CompileShadersErrors* errors)
{
    return nullptr;
}

UniquePtr<Texture> Device::CreateTexture(const TextureDesc& desc)
{
    return nullptr;
}

UniquePtr<DepthStencilState> gpu::Device::CreateDepthStencilState(const DepthStencilStateDesc & desc)
{
	return nullptr;
}

UniquePtr<DepthStencilState> CreateDepthStencilState(const DepthStencilStateDesc& desc)
{
	return nullptr;
}

#endif
