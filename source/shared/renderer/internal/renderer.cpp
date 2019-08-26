#include "pch.h"
#include "renderer.h"
#include "frame.h"
#include "geometryBuilder.h"
#include "geometryUtils.h"
#include "font.h"

using namespace rend;
using namespace math;

constexpr uint32 c_gridSize = 100;
constexpr uint32 c_gridNumVertices = c_gridSize * 2 * 2 + 4;

bool Renderer::Initialize(const RendererInitializeContext& context)
{
	// store screen settings, todo: refactor this out, should be passed to present frame as viewport info
	m_width = context.m_width;
	m_height = context.m_height;
	
	// create gpu device, one device to rule them all
	m_device = gpu::Device::CreateDefault();
	
	// create main command queue, a queue of command buffers, each of which contains rendering commands
	{
		gpu::CommandQueueDesc desc;
		m_mainCommandQueue = m_device->CreateCommandQueue(desc);
	}
	
	// compile basic shaders
	{
		FileStream shaderFile(context.m_shadersDirectoryPath + "simple.metal", FileStream::in);
		FUR_ASSERT(shaderFile);	// can't locate file
		String shaderCode((std::istreambuf_iterator<char>(shaderFile)), std::istreambuf_iterator<char>());
		
		// todo: add HLSL version
		const char* shadersSrc = shaderCode.c_str();
		
		gpu::CompileShadersErrors errors;
		m_library = m_device->CompileShaders(shadersSrc, gpu::CompileShadersOptions(), &errors);
		
		m_vertFunc = m_library->CreateShader("vertFunc");
		m_fragFunc = m_library->CreateShader("fragFunc");
		m_simpleVertexShader = m_library->CreateShader("simpleVertexShader");
		m_regularVertexShader = m_library->CreateShader("regularVertexShader");
		m_checkerFragmentShader = m_library->CreateShader("checkerFragmentShader");
		m_texturedFragmentShader = m_library->CreateShader("texturedFragmentShader");
		m_instancedVertexShader = m_library->CreateShader("instancedVertexShader");
		m_grassFragmentShader = m_library->CreateShader("grassFragmentShader");
		m_textVertexShader = m_library->CreateShader("textVertexShader");
		m_textFragmentShader = m_library->CreateShader("textFragmentShader");
		m_spriteVertexShader = m_library->CreateShader("spriteVertexShader");
		m_spriteFragmentShader = m_library->CreateShader("spriteFragmentShader");
	}
	
	// create basic render target, buffer to store output pixels
	{
		gpu::TextureDesc textureDesc;
		textureDesc.m_width = context.m_width * 2;
		textureDesc.m_height = context.m_height * 2;
		textureDesc.m_mipmapped = false;
		textureDesc.m_usage = gpu::TextureUsage::RenderTarget;
		textureDesc.m_format = gpu::PixelFormat::RGBA8Unorm;
		m_renderTarget = m_device->CreateTexture(textureDesc);
	}
	
	// create depth render target, buffer to store depth for each pixel
	{
		gpu::TextureDesc textureDesc;
		textureDesc.m_width = context.m_width * 2;
		textureDesc.m_height = context.m_height * 2;
		textureDesc.m_mipmapped = false;
		textureDesc.m_usage = gpu::TextureUsage::RenderTarget;
		textureDesc.m_storageMode = gpu::StorageMode::Private;		// for depth buffer the storage is required to be private
		textureDesc.m_format = gpu::PixelFormat::Depth24Unorm_Stencil8;
		m_depthRenderTarget = m_device->CreateTexture(textureDesc);
	}
	
	// create basic render pass descriptor - where to output pixels / data from this render pass, render targets setup
	{
		gpu::RenderPassDescriptorDesc desc;
		
		// albedo render target
		desc.m_colorAttachments[0].m_texture = m_renderTarget.get();
		desc.m_colorAttachments[0].m_loadAction = gpu::LoadAction::Clear;
		desc.m_colorAttachments[0].m_storeAction = gpu::StoreAction::Store;
		desc.m_colorAttachments[0].m_clearColor = {0.0f, 0.0f, 0.0f, 0.0f};
		
		// depth render target
		desc.m_depthAttachment.m_texture = m_depthRenderTarget.get();
		desc.m_depthAttachment.m_clearDepth = std::numeric_limits<double>::max();
		desc.m_depthAttachment.m_loadAction = gpu::LoadAction::Clear;
		desc.m_depthAttachment.m_storeAction = gpu::StoreAction::Store;
		
		m_renderPassDesc = gpu::RenderPassDescriptor::Create(desc);
	}
	
	// create ending main render pass descriptor
	{
		gpu::RenderPassDescriptorDesc desc;
		
		// albedo render target
		desc.m_colorAttachments[0].m_texture = m_renderTarget.get();
		desc.m_colorAttachments[0].m_loadAction = gpu::LoadAction::Load;
		desc.m_colorAttachments[0].m_storeAction = gpu::StoreAction::Store;
		
		// depth render target
		desc.m_depthAttachment.m_texture = m_depthRenderTarget.get();
		desc.m_depthAttachment.m_loadAction = gpu::LoadAction::Load;
		desc.m_depthAttachment.m_storeAction = gpu::StoreAction::Store;
		
		m_mainRenderPassDescEnd = gpu::RenderPassDescriptor::Create(desc);
	}
	
	// create starting render pass desc for UI, which will clear only depth render target
	{
		gpu::RenderPassDescriptorDesc desc;
		
		// albedo render target
		desc.m_colorAttachments[0].m_texture = m_renderTarget.get();
		desc.m_colorAttachments[0].m_loadAction = gpu::LoadAction::Load;
		desc.m_colorAttachments[0].m_storeAction = gpu::StoreAction::Store;
		
		// depth render target
		desc.m_depthAttachment.m_texture = m_depthRenderTarget.get();
		desc.m_depthAttachment.m_clearDepth = std::numeric_limits<double>::max();
		desc.m_depthAttachment.m_loadAction = gpu::LoadAction::Clear;
		desc.m_depthAttachment.m_storeAction = gpu::StoreAction::Store;
		
		m_renderUIPassDescClearDepth = gpu::RenderPassDescriptor::Create(desc);
	}
	
	// create basic render pipeline state - how geometry is drawn, what's its material
	{
		gpu::GraphicsPipelineStateDesc renderPipelineDesc;
		renderPipelineDesc.m_vertexShader = m_vertFunc.get();
		renderPipelineDesc.m_pixelShader = m_fragFunc.get();
		
		gpu::GraphicsPipelineStateColorAttachmentDesc colorAttachmentDesc;
		colorAttachmentDesc.m_pixelFormat = gpu::PixelFormat::RGBA8Unorm;
		renderPipelineDesc.m_colorAttachments = &colorAttachmentDesc;
		renderPipelineDesc.m_numColorAttachments = 1;
		
		m_renderPipelineState = m_device->CreateGraphicsPipelineState(renderPipelineDesc);
	}
	
	// create depth stencil state
	{
		gpu::DepthStencilStateDesc desc;
		desc.m_compareFunction = gpu::CompareFunction::Less;
		desc.m_isDepthWriteEnabled = true;
		m_depthStencilState = m_device->CreateDepthStencilState(desc);
	}
	
	// create 3D render pipeline state
	{
		gpu::GraphicsPipelineStateDesc renderPipelineDesc;
		renderPipelineDesc.m_vertexShader = m_simpleVertexShader.get();
		renderPipelineDesc.m_pixelShader = m_fragFunc.get();
		
		gpu::GraphicsPipelineStateColorAttachmentDesc colorAttachmentDesc;
		colorAttachmentDesc.m_pixelFormat = gpu::PixelFormat::RGBA8Unorm;
		renderPipelineDesc.m_colorAttachments = &colorAttachmentDesc;
		renderPipelineDesc.m_numColorAttachments = 1;
		
		m_renderDebugLines3DState = m_device->CreateGraphicsPipelineState(renderPipelineDesc);
	}
	
	// create 3D render mesh pipeline state
	{
		gpu::GraphicsPipelineStateDesc renderPipelineDesc;
		renderPipelineDesc.m_vertexShader = m_regularVertexShader.get();
		renderPipelineDesc.m_pixelShader = m_texturedFragmentShader.get();
		
		gpu::GraphicsPipelineStateColorAttachmentDesc colorAttachmentDesc;
		colorAttachmentDesc.m_pixelFormat = gpu::PixelFormat::RGBA8Unorm;
		renderPipelineDesc.m_colorAttachments = &colorAttachmentDesc;
		renderPipelineDesc.m_numColorAttachments = 1;
		
		m_renderMeshState = m_device->CreateGraphicsPipelineState(renderPipelineDesc);
	}
	
	// create 3D render grass pipeline state
	{
		gpu::GraphicsPipelineStateDesc renderPipelineDesc;
		renderPipelineDesc.m_vertexShader = m_instancedVertexShader.get();
		renderPipelineDesc.m_pixelShader = m_grassFragmentShader.get();
		
		gpu::GraphicsPipelineStateColorAttachmentDesc colorAttachmentDesc;
		colorAttachmentDesc.m_pixelFormat = gpu::PixelFormat::RGBA8Unorm;
		renderPipelineDesc.m_colorAttachments = &colorAttachmentDesc;
		renderPipelineDesc.m_numColorAttachments = 1;
		
		m_renderGrassState = m_device->CreateGraphicsPipelineState(renderPipelineDesc);
	}
	
	// create 3D render grass pipeline state
	{
		gpu::GraphicsPipelineStateDesc renderPipelineDesc;
		renderPipelineDesc.m_vertexShader = m_spriteVertexShader.get();
		renderPipelineDesc.m_pixelShader = m_spriteFragmentShader.get();
		
		gpu::GraphicsPipelineStateColorAttachmentDesc colorAttachmentDesc;
		colorAttachmentDesc.m_pixelFormat = gpu::PixelFormat::RGBA8Unorm;
		renderPipelineDesc.m_colorAttachments = &colorAttachmentDesc;
		renderPipelineDesc.m_numColorAttachments = 1;
		
		m_renderSpriteState = m_device->CreateGraphicsPipelineState(renderPipelineDesc);
	}
	
	// create 3D render instanced mesh pipeline state
	{
		gpu::GraphicsPipelineStateDesc renderPipelineDesc;
		renderPipelineDesc.m_vertexShader = m_instancedVertexShader.get();
		renderPipelineDesc.m_pixelShader = m_texturedFragmentShader.get();
		
		gpu::GraphicsPipelineStateColorAttachmentDesc colorAttachmentDesc;
		colorAttachmentDesc.m_pixelFormat = gpu::PixelFormat::RGBA8Unorm;
		renderPipelineDesc.m_colorAttachments = &colorAttachmentDesc;
		renderPipelineDesc.m_numColorAttachments = 1;
		
		m_renderInstancedMeshState = m_device->CreateGraphicsPipelineState(renderPipelineDesc);
	}
	
	// create text render state
	{
		gpu::GraphicsPipelineStateDesc renderPipelineDesc;
		renderPipelineDesc.m_vertexShader = m_textVertexShader.get();
		renderPipelineDesc.m_pixelShader = m_textFragmentShader.get();
		
		gpu::GraphicsPipelineStateColorAttachmentDesc colorAttachmentDesc;
		colorAttachmentDesc.m_pixelFormat = gpu::PixelFormat::RGBA8Unorm;
		renderPipelineDesc.m_colorAttachments = &colorAttachmentDesc;
		renderPipelineDesc.m_numColorAttachments = 1;
		
		m_renderTextState = m_device->CreateGraphicsPipelineState(renderPipelineDesc);
	}
	
	// create UI render state
	{
		gpu::GraphicsPipelineStateDesc renderPipelineDesc;
		renderPipelineDesc.m_vertexShader = m_textVertexShader.get();
		renderPipelineDesc.m_pixelShader = m_textFragmentShader.get();
		
		gpu::GraphicsPipelineStateColorAttachmentDesc colorAttachmentDesc;
		colorAttachmentDesc.m_pixelFormat = gpu::PixelFormat::RGBA8Unorm;
		renderPipelineDesc.m_colorAttachments = &colorAttachmentDesc;
		renderPipelineDesc.m_numColorAttachments = 1;
		
		m_renderTextState = m_device->CreateGraphicsPipelineState(renderPipelineDesc);
	}
	
	// create camera constant buffer
	{
		CameraCB data;
		m_cameraCB = m_device->CreateBuffer({&data, sizeof(CameraCB), gpu::ResourceOptions::CpuCacheModeDefaultCache});
	}
	
	// create object matrices constant buffer
	{
		ObjectMatricesCB data;
		data.m_modelToWorld = math::CreateMatrix4x4Identity();
		m_objectMatricesCB = m_device->CreateBuffer({&data, sizeof(ObjectMatricesCB), gpu::ResourceOptions::CpuCacheModeDefaultCache});
	}
	
	// create basic constant buffers, additional data buffer sent to shaders along with index and vertex buffers
	{
		// todo: refactor this out
		ConstantBufferData data;
		m_exampleCB = m_device->CreateBuffer({&data, sizeof(ConstantBufferData), gpu::ResourceOptions::CpuCacheModeDefaultCache});
	}
	
	// create example geometry, todo: refactor this out
	{
		const math::Vector3 vertexData[] =
		{
			{0.0f,  0.02f, 0.0f},
			{-0.02f, -0.02f, 0.0f},
			{0.02f, -0.02f, 0.0f},
		};
		
		GeometryBuilder builder;
		SimpleVertexLayout vertex;
		const uint32 numVertices = sizeof(vertexData) / sizeof(math::Vector3);
		for(uint32 i=0; i<numVertices; ++i)
		{
			vertex.m_position = vertexData[i];
			builder.AddVertex(vertex);
		}
		
		const VertexLayoutAttribute attributes[] =
		{
			VertexLayoutAttribute::Position,
		};
		
		builder.BuildGeometryBuffer(m_device.get(), attributes, ArraySize(attributes), 0, m_vertexBuffer, nullptr);
	}
	
	// create axes geometry
	{
		GeometryBuilder builder;
		shape::AddWorldAxesShape(builder, {0, 0, 0});
		
		const VertexLayoutAttribute attributes[] =
		{
			VertexLayoutAttribute::Position,
			VertexLayoutAttribute::Color,
		};
		
		builder.BuildGeometryBuffer(m_device.get(), attributes, ArraySize(attributes), 0, m_worldAxesVertices, nullptr);
	}
	
	// grid lines geometry
	{
		GeometryBuilder builder;
		
		constexpr float centerOffset = (float)c_gridSize * 0.5f;
		constexpr Vector3 color = {0.3f, 0.3f, 0.3f};
		
		// lines along Y axis
		{
			Vector3 point_y_start = {-(float)centerOffset, -(float)centerOffset, 0};
			Vector3 point_y_end = {-(float)centerOffset, (float)centerOffset, 0};
			
			for(uint32 x=0; x<c_gridSize; ++x)
			{
				shape::AddLine(builder, point_y_start, point_y_end, color);
				point_y_start.x += 1.0f;
				point_y_end.x += 1.0f;
			}
			
			shape::AddLine(builder, point_y_start, point_y_end, color);
		}
		
		// lines along X axis
		{
			Vector3 point_x_start = {-(float)centerOffset, -(float)centerOffset, 0};
			Vector3 point_x_end = {(float)centerOffset, -(float)centerOffset, 0};
			
			for(uint32 y=0; y<c_gridSize; ++y)
			{
				shape::AddLine(builder, point_x_start, point_x_end, color);
				point_x_start.y += 1.0f;
				point_x_end.y += 1.0f;
			}
			
			shape::AddLine(builder, point_x_start, point_x_end, color);
		}
		
		const VertexLayoutAttribute attributes[] =
		{
			VertexLayoutAttribute::Position,
			VertexLayoutAttribute::Color,
		};
		
		builder.BuildGeometryBuffer(m_device.get(), attributes, ArraySize(attributes), 0, m_gridVertices, nullptr);
		
	}
	
	// create font and text buffer
	{
		m_font = UniquePtr<Font>(Font::LoadFont(context.m_fontPath.c_str(), m_device.get()));
		
		const char* text = "Duena";
		StringView textView(text);
		
		m_numTextVertices = Font::CalculateNumVerticesForText((uint32)textView.Size());
		gpu::BufferDesc desc;
		desc.m_data = nullptr;
		desc.m_size = m_numTextVertices * sizeof(GlyphVertex);
		
		m_textVertices = m_device->CreateBuffer(desc);
		
		ArrayView<GlyphVertex> vertices(reinterpret_cast<GlyphVertex*>(m_textVertices->GetContent()), m_numTextVertices);
		
		constexpr float textScale = 10.0f;
		
		rend::FillTextVerticesFor(m_font.get(), textView, vertices,	textScale);
	}
	
	// create text matrix buffer
	{
		gpu::BufferDesc desc;
		desc.m_data = nullptr;
		desc.m_size = sizeof(ObjectMatricesCB);
		m_textMatrixBuffer = m_device->CreateBuffer(desc);
		
		m_textMatrix = math::CreateMatrix4x4RotationY(180.0_deg) * math::CreateMatrix4x4Translation(0.0f, 0.0f, 1.0f);
		
		ObjectMatricesCB* obj = reinterpret_cast<ObjectMatricesCB*>(m_textMatrixBuffer->GetContent());
		obj->m_modelToWorld = m_textMatrix;
	}
	
	// load sprite library
	{
		m_testSpriteLibrary = UniquePtr<SpriteLibrary>(SpriteLibrary::LoadSpriteLibrary(context.m_spriteLibraryPath.c_str(), m_device.get()));
	}
	
	return true;
}

float Renderer::GetAspectRatio() const
{
	return (float)m_width / (float)m_height;
}

void Renderer::DrawFrame(const Frame* frame)
{
	// create camera matrices based on camera parameters
	{
		CameraCB* camera = reinterpret_cast<CameraCB*>(m_cameraCB->GetContent());
		camera->m_projection = math::Transpose(math::CreateMatrix4x4Projection(frame->m_camera.m_fov, GetAspectRatio(), 0.1f, 1000.0f));
		
		// rotating worldToCamera matrix because of Metal's coodrinate system (see more in math comments)
		camera->m_worldToCamera = math::Transpose(math::CreateMatrix4x4RotationX(-math::c_pi_2) *		// rotate -90 degrees because of different coordinate systems
												  math::CreateMatrix4x4LookAt(frame->m_camera.m_eye, frame->m_camera.m_at, frame->m_camera.m_up)
												  );
	}
	
	// draw scene to separate render target
	{
		gpu::CommandBuffer commandBuffer = m_mainCommandQueue->CreateCommandBuffer();
		
		// copy objects data into constant buffer, could be optimized in future to avoid copying,
		// but would require to expose constant buffer, not a problem if will be wrapped in something
		ConstantBufferData* content = reinterpret_cast<ConstantBufferData*>(m_exampleCB->GetContent());
		*content = *frame->m_objectsData;
		
		gpu::ParallelRenderCommandEncoder parallelRenderCommandEncoder = commandBuffer.CreateParallelRenderCommandEncoder(m_renderPassDesc.get());
		gpu::GraphicsCommandEncoder renderCommandEncoder = parallelRenderCommandEncoder.CreateRenderCommandEncoder();
		renderCommandEncoder.SetRenderPipelineState(m_renderPipelineState.get());
		renderCommandEncoder.SetDepthStencilState(m_depthStencilState.get());
		renderCommandEncoder.SetVertexBuffer(m_vertexBuffer.get(), 0, 0);
		renderCommandEncoder.SetVertexBuffer(m_exampleCB.get(), 0, 1);
		renderCommandEncoder.Draw(gpu::PrimitiveType::Triangle, 0, 3 * frame->m_objectsCount);
		
		renderCommandEncoder.EndEncoding();
		parallelRenderCommandEncoder.EndEncoding();
		
		commandBuffer.Commit();
	}
	
	// draw grid
	{
		gpu::CommandBuffer commandBuffer = m_mainCommandQueue->CreateCommandBuffer();
		gpu::GraphicsCommandEncoder renderCommandEncoder = commandBuffer.CreateGraphicsCommandEncoder(m_mainRenderPassDescEnd.get());
		
		renderCommandEncoder.SetRenderPipelineState(m_renderDebugLines3DState.get());
		renderCommandEncoder.SetDepthStencilState(m_depthStencilState.get());
		renderCommandEncoder.SetVertexBuffer(m_gridVertices.get(), 0, 0);
		renderCommandEncoder.SetVertexBuffer(m_cameraCB.get(), 0, 1);
		renderCommandEncoder.Draw(gpu::PrimitiveType::Line, 0, c_gridNumVertices);
		
		renderCommandEncoder.EndEncoding();
		
		commandBuffer.Commit();
	}
	
	// draw 3D world axes
	{
		gpu::CommandBuffer commandBuffer = m_mainCommandQueue->CreateCommandBuffer();
		gpu::GraphicsCommandEncoder renderCommandEncoder = commandBuffer.CreateGraphicsCommandEncoder(m_mainRenderPassDescEnd.get());
		
		renderCommandEncoder.SetRenderPipelineState(m_renderDebugLines3DState.get());
		renderCommandEncoder.SetDepthStencilState(m_depthStencilState.get());
		renderCommandEncoder.SetVertexBuffer(m_worldAxesVertices.get(), 0, 0);
		renderCommandEncoder.SetVertexBuffer(m_cameraCB.get(), 0, 1);
		renderCommandEncoder.Draw(gpu::PrimitiveType::Line, 0, 6);
		
		renderCommandEncoder.EndEncoding();
		
		commandBuffer.Commit();
	}
	
	// draw grass
	{
		
	}
	
	// draw instanced debug meshes
	if(frame->m_testMesh != nullptr && frame->m_testMesh->m_chunks.empty() == false)
	{
		gpu::CommandBuffer commandBuffer = m_mainCommandQueue->CreateCommandBuffer();
		gpu::GraphicsCommandEncoder renderCommandEncoder = commandBuffer.CreateGraphicsCommandEncoder(m_mainRenderPassDescEnd.get());
		
		const MeshChunk& chunk = frame->m_testMesh->m_chunks[0];
		
		renderCommandEncoder.SetRenderPipelineState(m_renderGrassState.get());
		renderCommandEncoder.SetDepthStencilState(m_depthStencilState.get());
		renderCommandEncoder.SetVertexBuffer(&chunk.m_vertices, 0, 0);
		renderCommandEncoder.SetVertexBuffer(&chunk.m_indices, 0, 1);
		renderCommandEncoder.SetVertexBuffer(m_cameraCB.get(), 0, 2);
		renderCommandEncoder.SetVertexBuffer(frame->m_testInstanceMatrices, 0, 3);
		renderCommandEncoder.SetVertexBuffer(frame->m_testInstanceColors, 0, 4);
		renderCommandEncoder.SetFragmentTexture(frame->m_testAlbedoTexture, 0);
		renderCommandEncoder.SetFragmentTexture(frame->m_testAmbientOcclusionTexture, 1);
		renderCommandEncoder.Draw(gpu::PrimitiveType::Triangle, 0, chunk.m_numIndices, frame->m_testNumInstances);
		
		renderCommandEncoder.EndEncoding();
		
		commandBuffer.Commit();
	}
	
	// draw text 3D
	{
		static float angle = 0.0f;
		angle += 0.01f;
		m_textMatrix = math::CreateMatrix4x4RotationY(180.0_deg) * math::CreateMatrix4x4RotationZ(angle) * math::CreateMatrix4x4Translation(0.0f, 0.0f, 1.0f);
		
		ObjectMatricesCB* obj = reinterpret_cast<ObjectMatricesCB*>(m_textMatrixBuffer->GetContent());
		obj->m_modelToWorld = m_textMatrix;
		
		gpu::CommandBuffer commandBuffer = m_mainCommandQueue->CreateCommandBuffer();
		gpu::GraphicsCommandEncoder renderCommandEncoder = commandBuffer.CreateGraphicsCommandEncoder(m_mainRenderPassDescEnd.get());
		
		renderCommandEncoder.SetRenderPipelineState(m_renderTextState.get());
		renderCommandEncoder.SetDepthStencilState(m_depthStencilState.get());
		renderCommandEncoder.SetVertexBuffer(m_textVertices.get(), 0, 0);
		renderCommandEncoder.SetVertexBuffer(m_cameraCB.get(), 0, 1);
		renderCommandEncoder.SetVertexBuffer(m_textMatrixBuffer.get(), 0, 2);
		renderCommandEncoder.SetFragmentTexture(m_font->GetAtlasTexture(), 0);
		renderCommandEncoder.Draw(gpu::PrimitiveType::Triangle, 0, m_numTextVertices);
		
		renderCommandEncoder.EndEncoding();
		
		commandBuffer.Commit();
	}
	
	// draw sprites
	{
		gpu::CommandBuffer commandBuffer = m_mainCommandQueue->CreateCommandBuffer();
		gpu::GraphicsCommandEncoder renderCommandEncoder = commandBuffer.CreateGraphicsCommandEncoder(m_renderUIPassDescClearDepth.get());
		
		renderCommandEncoder.SetRenderPipelineState(m_renderSpriteState.get());
		renderCommandEncoder.SetDepthStencilState(m_depthStencilState.get());
		renderCommandEncoder.SetVertexBuffer(m_testSpriteLibrary->GetFramesUVs(), 0, 0);
		renderCommandEncoder.SetVertexBuffer(m_cameraCB.get(), 0, 1);
		renderCommandEncoder.SetVertexBuffer(frame->m_testSpriteInstances, 0, 2);
		renderCommandEncoder.SetFragmentTexture(m_testSpriteLibrary->GetAtlasTexture(), 0);
		renderCommandEncoder.Draw(gpu::PrimitiveType::Triangle, 0, 6, frame->m_testNumSpriteInstances);
		
		renderCommandEncoder.EndEncoding();
		
		commandBuffer.Commit();
	}
}

void Renderer::PresentFrame(gpu::Drawable* drawable, gpu::RenderPassDescriptor* passDescriptor)
{
	// present render target to screen
	{
		gpu::CommandBuffer commandBuffer = m_mainCommandQueue->CreateCommandBuffer();
		
		if (*passDescriptor)
		{
			gpu::BlitCommandEncoder blitCommandEncoder = commandBuffer.CreateBlitCommandEncoder();
			blitCommandEncoder.Copy(*m_renderTarget.get(), 0, 0, {0, 0, 0}, {m_width * 2, m_height * 2, 1}, passDescriptor->GetColorAttachmentTexture(0), 0, 0, {0, 0, 0});
			
			blitCommandEncoder.EndEncoding();
			commandBuffer.Present(drawable);
		}
		
		commandBuffer.Commit();
		commandBuffer.WaitUntilCompleted();
		
	}
}

const SpriteLibrary* Renderer::GetTestSpriteLibrary() const
{
	return m_testSpriteLibrary.get();
}
