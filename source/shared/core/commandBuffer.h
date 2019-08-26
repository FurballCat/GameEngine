#pragma once

namespace fur
{
	struct CommandBuffer
	{
		void* m_data;
		uint32 m_size;
		uint32 m_capacity;
	};
	
	typedef void (*CommandFunc)(void* ctx, const void* cmd);
	
	class CORE_API CommandEncoder
	{
	public:
		CommandEncoder(CommandBuffer* buffer);
		
		template<typename T>
		void Commit(const T& cmd)
		{
			CommandFunc func = T::Func;
			EncodeCommand(func, &cmd, sizeof(T));
		}
		
	private:
		void EncodeCommand(CommandFunc func, const void* data, uint32 size);
		
		CommandBuffer* m_buffer;
	};
	
	CORE_API void ExecuteCommands(const CommandBuffer* buffer, void* ctx);
}
