#pragma once

namespace gpu
{
    class Buffer;
    struct BufferDesc;
    class CommandQueue;
    struct CommandQueueDesc;
    class GraphicsPipelineState;
    struct GraphicsPipelineStateDesc;
    class ShaderLibrary;
    struct CompileShadersOptions;
    struct CompileShadersErrors;
    class Texture;
    struct TextureDesc;
	class DepthStencilState;
	struct DepthStencilStateDesc;
    
    class GPU_API Device
    {
        DECLARE_GPU_PLATFORM_PTR(Device, m_ptr);
    public:
        static UniquePtr<Device> CreateDefault();
        
        UniquePtr<Buffer> CreateBuffer(const BufferDesc& desc);
		bool CreateBuffer(const BufferDesc& desc, Buffer& outBuffer);
        UniquePtr<CommandQueue> CreateCommandQueue(const CommandQueueDesc& desc);
        UniquePtr<GraphicsPipelineState> CreateGraphicsPipelineState(const GraphicsPipelineStateDesc& desc);
        UniquePtr<ShaderLibrary> CompileShaders(const char* sourceCode, const CompileShadersOptions& options, CompileShadersErrors* errors = nullptr);
        UniquePtr<Texture> CreateTexture(const TextureDesc& desc);
		UniquePtr<DepthStencilState> CreateDepthStencilState(const DepthStencilStateDesc& desc);
    };
}
