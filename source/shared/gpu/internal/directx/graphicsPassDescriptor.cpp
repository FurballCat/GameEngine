#include "pch.h"
#include "graphicsPassDescriptor.h"
#include "texture.h"

using namespace gpu;

#ifdef PLATFORM_WINDOWS

RenderPassDescriptor::RenderPassDescriptor()
{
    
}

UniquePtr<RenderPassDescriptor> RenderPassDescriptor::Create(RenderPassDescriptorDesc& desc)
{
    return nullptr;
}

Texture RenderPassDescriptor::GetColorAttachmentTexture(uint32 index)
{
	return Texture(nullptr);
}

#endif
