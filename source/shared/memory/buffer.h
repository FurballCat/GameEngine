#pragma once

namespace mem
{
	struct BinaryBuffer
	{
		uint8* m_data;
		uint32 m_size;
		Tag m_tag;
		
		static constexpr BinaryBuffer EMPTY() { return {nullptr, 0}; }
	};
	
	CORE_API void AllocateBinaryBuffer(BinaryBuffer& buffer, uint32 size, Tag tag);
	CORE_API void ReleaseBinaryBuffer(BinaryBuffer& buffer, Tag tag);
}
