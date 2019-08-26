#include "pch.h"
#include "core/public.h"
#include "core/commandBuffer.h"
#include "unitTests/unitTestsFramework.h"

using namespace test;

// example command buffer implementation
struct ExampleCommandContext
{
	float m_result;
};

struct ExampleCommand_Add
{
	float m_value;
	
	static void Func(void* ctx, const void* cmd)
	{
		ExampleCommandContext* context = reinterpret_cast<ExampleCommandContext*>(ctx);
		const ExampleCommand_Add* command = reinterpret_cast<const ExampleCommand_Add*>(cmd);
		context->m_result += command->m_value;
	}
};

struct ExampleCommand_Mul
{
	float m_value;
	
	static void Func(void* ctx, const void* cmd)
	{
		ExampleCommandContext* context = reinterpret_cast<ExampleCommandContext*>(ctx);
		const ExampleCommand_Mul* command = reinterpret_cast<const ExampleCommand_Mul*>(cmd);
		context->m_result *= command->m_value;
	}
};

UNITTEST(CommandBufferTests, CallSimpleCommand)
{
	constexpr uint32 DATA_SIZE = 1024;
	uint8 data[DATA_SIZE];
	fur::CommandBuffer buffer = {data, 0, DATA_SIZE};
	
	fur::CommandEncoder encoder(&buffer);
	
	encoder.Commit(ExampleCommand_Add{20.0f});
	encoder.Commit(ExampleCommand_Add{1.0f});
	encoder.Commit(ExampleCommand_Mul{2.0f});
	
	ExampleCommandContext ctx = {0.0f};
	fur::ExecuteCommands(&buffer, &ctx);
	
	Assert::AreEqual(ctx.m_result, 42.0f);
}
