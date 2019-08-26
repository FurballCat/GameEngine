#include "pch.h"
#include "buffer.h"

using namespace mem;

void mem::AllocateBinaryBuffer(mem::BinaryBuffer& buffer, uint32 size, Tag tag)
{
	FUR_ASSERT(buffer.m_data == nullptr);
	buffer = { Allocate(size, tag), size, tag };
}

void mem::ReleaseBinaryBuffer(BinaryBuffer& buffer, Tag tag)
{
	Free(buffer.m_data, buffer.m_tag);
	buffer.m_data = nullptr;
	buffer.m_size = 0;
	buffer.m_tag = Tag::None;
}
