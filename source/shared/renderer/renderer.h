#pragma once

namespace gpu
{
	class Device;
	class CommandQueue;
	class DepthStencilState;
}

namespace Utility
{
	namespace TTFCore
	{
		class Font;
	}
}

namespace rend
{
	struct Frame;
	class Font;
	class SpriteLibrary;
	
	struct RendererInitializeContext
	{
		uint32 m_width;
		uint32 m_height;
		String m_shadersDirectoryPath;
		String m_fontPath;
		String m_spriteLibraryPath;
	};
	
	struct CameraCB
	{
		math::Matrix4x4 m_worldToCamera;
		math::Matrix4x4 m_projection;
	};
	
	struct ObjectMatricesCB
	{
		math::Matrix4x4 m_modelToWorld;
	};
	
	struct TextVertex
	{
		math::Vector2 m_position;
	};
	
	class RENDERER_API Renderer
	{
	public:
		bool Initialize(const RendererInitializeContext& context);
		
		void DrawFrame(const Frame* frame);
		void PresentFrame(gpu::Drawable* drawable, gpu::RenderPassDescriptor* passDescriptor);
		
		// todo: rethink that, maybe device is not a member of renderer?
		gpu::Device* GetDevice() { return m_device.get(); }
		const gpu::Device* GetDevice() const { return m_device.get(); }
		
		const SpriteLibrary* GetTestSpriteLibrary() const;
		
	private:
		float GetAspectRatio() const;
		
		UniquePtr<gpu::Device> m_device;
		UniquePtr<gpu::CommandQueue> m_mainCommandQueue;
		
		// screen settings, todo: refactor this out, should be in viewport data or somethings, check old code
		uint32 m_width;
		uint32 m_height;
		
		// basic render target
		UniquePtr<gpu::Texture> m_renderTarget;
		
		// camera matrices
		UniquePtr<gpu::Buffer> m_cameraCB;
		
		// object matrices
		UniquePtr<gpu::Buffer> m_objectMatricesCB;
		
		// basic render pass and render state
		UniquePtr<gpu::GraphicsPipelineState> m_renderPipelineState;
		UniquePtr<gpu::RenderPassDescriptor> m_renderPassDesc;
		UniquePtr<gpu::RenderPassDescriptor> m_renderUIPassDescClearDepth;
		
		// depth and stencil
		UniquePtr<gpu::Texture> m_depthRenderTarget;
		UniquePtr<gpu::DepthStencilState> m_depthStencilState;
		
		// 3D debug lines
		UniquePtr<gpu::GraphicsPipelineState> m_renderDebugLines3DState;
		UniquePtr<gpu::RenderPassDescriptor> m_mainRenderPassDescEnd;
		
		UniquePtr<gpu::Shader> m_simpleVertexShader;
		
		// basic shaders
		UniquePtr<gpu::ShaderLibrary> m_library;
		UniquePtr<gpu::Shader> m_vertFunc;
		UniquePtr<gpu::Shader> m_fragFunc;
		
		// mesh vertex shader
		UniquePtr<gpu::Shader> m_regularVertexShader;
		UniquePtr<gpu::Shader> m_instancedVertexShader;
		UniquePtr<gpu::Shader> m_checkerFragmentShader;
		UniquePtr<gpu::Shader> m_texturedFragmentShader;
		UniquePtr<gpu::Shader> m_grassFragmentShader;
		UniquePtr<gpu::Shader> m_spriteVertexShader;
		UniquePtr<gpu::Shader> m_spriteFragmentShader;
		
		UniquePtr<gpu::GraphicsPipelineState> m_renderSpriteState;
		UniquePtr<gpu::GraphicsPipelineState> m_renderGrassState;
		UniquePtr<gpu::GraphicsPipelineState> m_renderMeshState;
		UniquePtr<gpu::GraphicsPipelineState> m_renderInstancedMeshState;
		
		// text rendering state
		UniquePtr<gpu::GraphicsPipelineState> m_renderTextState;
		UniquePtr<gpu::Shader> m_textVertexShader;
		UniquePtr<gpu::Shader> m_textFragmentShader;
		
		// example constant buffer, todo: refactor this out
		UniquePtr<gpu::Buffer> m_exampleCB;
		
		// example geometry
		UniquePtr<gpu::Buffer> m_vertexBuffer;
		UniquePtr<gpu::Buffer> m_worldAxesVertices;
		
		// grid debug lines
		UniquePtr<gpu::Buffer> m_gridVertices;
		
		// text buffer
		UniquePtr<gpu::Buffer> m_textVertices;
		uint32 m_numTextVertices = 0;
		UniquePtr<gpu::Buffer> m_textMatrixBuffer;
		math::Matrix4x4 m_textMatrix;
		
		// font
		UniquePtr<Font> m_font;
		
		// test sprite library
		UniquePtr<SpriteLibrary> m_testSpriteLibrary;
	};
}
