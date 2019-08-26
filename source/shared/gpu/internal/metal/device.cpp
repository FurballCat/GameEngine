#include "pch.h"
#include "device.h"
#include "buffer.h"
#include "commandQueue.h"
#include "graphicsPipelineState.h"
#include "shader.h"
#include "shaderLibrary.h"
#include "texture.h"
#include "enums.h"
#include "depthStencilState.h"

using namespace gpu;

#ifdef PLATFORM_OSX

UniquePtr<Device> Device::CreateDefault()
{
    Device* device = new Device(mtlpp::Device::CreateSystemDefaultDevice());
    return UniquePtr<Device>(device);
}

UniquePtr<Buffer> Device::CreateBuffer(const gpu::BufferDesc& desc)
{
	Buffer* buffer = nullptr;
	
	if(desc.m_data)
		buffer = new Buffer(m_ptr.NewBuffer(desc.m_data, desc.m_size, ToNativeEnum(desc.m_options)));
	else
		buffer = new Buffer(m_ptr.NewBuffer(desc.m_size, ToNativeEnum(desc.m_options)));
	
    return UniquePtr<Buffer>(buffer);
}

bool Device::CreateBuffer(const BufferDesc& desc, Buffer& outBuffer)
{
	if(desc.m_data)
		outBuffer = Buffer(m_ptr.NewBuffer(desc.m_data, desc.m_size, ToNativeEnum(desc.m_options)));
	else
		outBuffer = Buffer(m_ptr.NewBuffer(desc.m_size, ToNativeEnum(desc.m_options)));
	
	return true;
}

UniquePtr<CommandQueue> Device::CreateCommandQueue(const CommandQueueDesc& desc)
{
    CommandQueue* commandQueue = new CommandQueue(m_ptr.NewCommandQueue());
    return UniquePtr<CommandQueue>(commandQueue);
}

UniquePtr<GraphicsPipelineState> Device::CreateGraphicsPipelineState(const GraphicsPipelineStateDesc& desc)
{
    mtlpp::RenderPipelineDescriptor mtlDesc;
    
    if(desc.m_vertexShader)
        mtlDesc.SetVertexFunction(desc.m_vertexShader->GetPlatformPtr());
    
    if(desc.m_pixelShader)
        mtlDesc.SetFragmentFunction(desc.m_pixelShader->GetPlatformPtr());
    
	for(uint32 i=0; i<desc.m_numColorAttachments; ++i)
	{
		mtlDesc.GetColorAttachments()[i].SetPixelFormat(ToNativeEnum(desc.m_colorAttachments[i].m_pixelFormat));
	}
    
    GraphicsPipelineState* state = new GraphicsPipelineState(m_ptr.NewRenderPipelineState(mtlDesc, nullptr));
    return UniquePtr<GraphicsPipelineState>(state);
}

UniquePtr<ShaderLibrary> Device::CompileShaders(const char* sourceCode, const CompileShadersOptions& options, CompileShadersErrors* errors)
{
    ns::Error error;
    ShaderLibrary* shaderLibrary = new ShaderLibrary(m_ptr.NewLibrary( sourceCode, mtlpp::CompileOptions(), &error));
	
	if(error.GetCode() != 0)
		FUR_LOG_ERROR(error.GetLocalizedDescription().GetCStr());
	
	FUR_ASSERT(shaderLibrary->GetPlatformPtr());		// check log for errors
	
    return UniquePtr<ShaderLibrary>(shaderLibrary);
}

UniquePtr<Texture> Device::CreateTexture(const TextureDesc& desc)
{
    mtlpp::TextureDescriptor mtlppDesc = mtlpp::TextureDescriptor::Texture2DDescriptor(ToNativeEnum(desc.m_format), desc.m_width, desc.m_height, desc.m_mipmapped);
    mtlppDesc.SetUsage(ToNativeEnum(desc.m_usage));
	mtlppDesc.SetStorageMode(ToNativeEnum(desc.m_storageMode));
    
    Texture* texture = new Texture(m_ptr.NewTexture(mtlppDesc));
    return UniquePtr<Texture>(texture);
}

UniquePtr<DepthStencilState> Device::CreateDepthStencilState(const DepthStencilStateDesc& desc)
{
	mtlpp::DepthStencilDescriptor mtlppDesc = mtlpp::DepthStencilDescriptor();
	mtlppDesc.SetDepthCompareFunction(ToNativeEnum(desc.m_compareFunction));
	mtlppDesc.SetDepthWriteEnabled(desc.m_isDepthWriteEnabled);
	DepthStencilState* state = new DepthStencilState(m_ptr.NewDepthStencilState(mtlppDesc));
	return UniquePtr<DepthStencilState>(state);
}

#endif
