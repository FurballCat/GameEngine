#include "pch.h"
#include "commandBuffer.h"

using namespace fur;

CommandEncoder::CommandEncoder(CommandBuffer* buffer)
	: m_buffer(buffer)
{
	
}

void CommandEncoder::EncodeCommand(CommandFunc func, const void* data, uint32 size)
{
	if(m_buffer)
	{
		FUR_ASSERT(m_buffer->m_size + sizeof(func) + sizeof(uint32) + size < m_buffer->m_capacity);
		
		uint32 offset = m_buffer->m_size;
		
		// encode function ptr
		std::memcpy((uint8*)m_buffer->m_data + offset, &func, sizeof(func));
		offset += sizeof(func);
		
		// encode data size
		std::memcpy((uint8*)m_buffer->m_data + offset, &size, sizeof(uint32));
		offset += sizeof(uint32);
		
		// encode data
		std::memcpy((uint8*)m_buffer->m_data + offset, data, size);
		offset += size;
		
		m_buffer->m_size = offset;
	}
}

void fur::ExecuteCommands(const CommandBuffer* buffer, void* ctx)
{
	uint32 size = buffer->m_size;
	const uint8* data = reinterpret_cast<const uint8*>(buffer->m_data);
	const uint8* dataEnd = data + size;
	
	while(data < dataEnd)
	{
		CommandFunc func = *reinterpret_cast<const CommandFunc*>(data);
		data += sizeof(CommandFunc);
		uint32 cmdSize = *reinterpret_cast<const uint32*>(data);
		data += sizeof(uint32);
		const void* cmdData = data;
		data += cmdSize;
		
		func(ctx, cmdData);
	}
}
