#pragma once

#define DECLARE_GPU_PLATFORM_PTR(_class, _member) \
public: \
    using PlatformPtr = ::gpu::_class##PlatformPtr;    \
    _class(PlatformPtr ptr) : m_ptr(ptr) {} \
    PlatformPtr GetPlatformPtr() { return _member; }    \
    const PlatformPtr GetPlatformPtr() const { return _member; }    \
    operator bool() { return _member; }   \
    bool IsValid() { return _member; }   \
private:    \
    PlatformPtr _member;

#define GPU_PLATFORM_PTR(_class, _type) \
namespace gpu   \
{   \
    using _class##PlatformPtr = _type; \
}

#ifdef PLATFORM_WINDOWS
    
    GPU_PLATFORM_PTR(Device, void*);    // ComPtr<ID3D12Device>
    GPU_PLATFORM_PTR(Buffer, void*);    // ComPtr<ID3D12Resource>
    GPU_PLATFORM_PTR(Shader, void*);    // ComPtr<ID3DBlob>
    GPU_PLATFORM_PTR(GraphicsPipelineState, void*);    // ComPtr<ID3D12PipelineState>
    GPU_PLATFORM_PTR(ComputePipelineState, void*);     // ComPtr<ID3D11PipelineState> note: same as GraphicsPipelineState
    GPU_PLATFORM_PTR(CommandQueue, void*);    // ComPtr<ID3D12CommandQueue>
    GPU_PLATFORM_PTR(ShaderLibrary, void*);   // note: not sure whether there is an equivalent of shader library in DirectX 12 or not
    GPU_PLATFORM_PTR(CommandBuffer, void*);   // not sure what's the DirectX equivalent
    GPU_PLATFORM_PTR(GraphicsCommandEncoder, void*);   // not sure what's the DirectX equivalent
	GPU_PLATFORM_PTR(ParallelRenderCommandEncoder, void*);   // not sure what's the DirectX equivalent
    GPU_PLATFORM_PTR(RenderPassDescriptor, void*);   // not sure what's the DirectX equivalent
    GPU_PLATFORM_PTR(Drawable, void*);
    GPU_PLATFORM_PTR(Texture, void*);
    GPU_PLATFORM_PTR(BlitCommandEncoder, void*);
	GPU_PLATFORM_PTR(DepthStencilState, void*);
    
#elif PLATFORM_OSX
    
    GPU_PLATFORM_PTR(Device, mtlpp::Device);
    GPU_PLATFORM_PTR(Buffer, mtlpp::Buffer);
    GPU_PLATFORM_PTR(Shader, mtlpp::Function);
    GPU_PLATFORM_PTR(GraphicsPipelineState, mtlpp::RenderPipelineState);
    GPU_PLATFORM_PTR(ComputePipelineState, mtlpp::ComputePipelineState);
    GPU_PLATFORM_PTR(CommandQueue, mtlpp::CommandQueue);
    GPU_PLATFORM_PTR(ShaderLibrary, mtlpp::Library);
    GPU_PLATFORM_PTR(CommandBuffer, mtlpp::CommandBuffer);
    GPU_PLATFORM_PTR(GraphicsCommandEncoder, mtlpp::RenderCommandEncoder);
	GPU_PLATFORM_PTR(ParallelRenderCommandEncoder, mtlpp::ParallelRenderCommandEncoder);
    GPU_PLATFORM_PTR(RenderPassDescriptor, mtlpp::RenderPassDescriptor);
    GPU_PLATFORM_PTR(Drawable, mtlpp::Drawable);
    GPU_PLATFORM_PTR(Texture, mtlpp::Texture);
    GPU_PLATFORM_PTR(BlitCommandEncoder, mtlpp::BlitCommandEncoder);
	GPU_PLATFORM_PTR(DepthStencilState, mtlpp::DepthStencilState);
    
#endif
