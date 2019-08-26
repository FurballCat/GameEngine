#include "pch.h"
#include "graphicsPassDescriptor.h"
#include "texture.h"
#include "vertexBufferLayout.h"

using namespace gpu;

#ifdef PLATFORM_OSX

RenderPassDescriptor::RenderPassDescriptor()
{
    
}

UniquePtr<RenderPassDescriptor> RenderPassDescriptor::Create(RenderPassDescriptorDesc& desc)
{
    RenderPassDescriptor* result = new RenderPassDescriptor();
    
    for(uint32 i=0; i<c_maxColorAttachments; ++i)
    {
        auto& attachment = desc.m_colorAttachments[i];
        if(attachment.m_texture)
        {
            result->m_ptr.GetColorAttachments()[i].SetTexture(attachment.m_texture->GetPlatformPtr());
            result->m_ptr.GetColorAttachments()[i].SetLoadAction(ToNativeEnum(attachment.m_loadAction));
            result->m_ptr.GetColorAttachments()[i].SetStoreAction(ToNativeEnum(attachment.m_storeAction));
            result->m_ptr.GetColorAttachments()[i].SetClearColor(mtlpp::ClearColor(attachment.m_clearColor.r, attachment.m_clearColor.g,
                                                                                   attachment.m_clearColor.b, attachment.m_clearColor.a));
        }
    }
	
	if(desc.m_depthAttachment.m_texture)
	{
		auto& attachment = desc.m_depthAttachment;
		result->m_ptr.GetDepthAttachment().SetTexture(attachment.m_texture->GetPlatformPtr());
		result->m_ptr.GetDepthAttachment().SetLoadAction(ToNativeEnum(attachment.m_loadAction));
		result->m_ptr.GetDepthAttachment().SetStoreAction(ToNativeEnum(attachment.m_storeAction));
		result->m_ptr.GetDepthAttachment().SetClearDepth(attachment.m_clearDepth);
	}
	
	if(desc.m_vertexDescription != nullptr)
	{
		mtlpp::VertexDescriptor vertexDesc;
		const auto& gpuVertexDesc = desc.m_vertexDescription;
		
		const auto& layouts = gpuVertexDesc->m_layouts;
		for(uint32 i=0; i<gpuVertexDesc->m_numLayouts; ++i)
		{
			vertexDesc.GetLayouts()[i].SetStride(layouts[i].m_stride);
			vertexDesc.GetLayouts()[i].SetStepRate(layouts[i].m_stepRate);
			vertexDesc.GetLayouts()[i].SetStepFunction(ToNativeEnum(layouts[i].m_stepFunction));
		}
		
		const auto& attributes = gpuVertexDesc->m_attributes;
		for(uint32 i=0; i<gpuVertexDesc->m_numAttributes; ++i)
		{
			vertexDesc.GetAttributes()[i].SetFormat(ToNativeEnum(attributes[i].m_format));
			vertexDesc.GetAttributes()[i].SetOffset(attributes[i].m_offset);
			vertexDesc.GetAttributes()[i].SetBufferIndex(attributes[i].m_bufferIndex);
		}
	}
	
    return UniquePtr<RenderPassDescriptor>(result);
}

Texture RenderPassDescriptor::GetColorAttachmentTexture(uint32 index)
{
	return Texture(m_ptr.GetColorAttachments()[index].GetTexture());
}

#endif
