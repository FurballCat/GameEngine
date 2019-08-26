#include "pch.h"
#include "core/api.h"
#include "core/types.h"
#include "core/variant.h"
#include "core/name.h"
#include "unitTests/unitTestsFramework.h"

namespace script
{
	typedef uint32 NcRegisterIndex;

	enum class NcOperation
	{
		// Data manipulation operations
		AssignToRegister,
		AssignFromRegister,

		// Math operations
		AddRegisters,
		SubtractRegisters,
		MultiplyRegisters,
		DivideRegisters,

		// Function calling
		Return,
		CallScriptFunction,
		CallNativeFunction,
	};

	class NcInstruction
	{
	public:
		NcOperation GetOperation()
		{
			return NcOperation::AddRegisters;
		}

		Variant& GetImmidiateValue()
		{
			static Variant value(0.0f);
			return value;
		}

		NcRegisterIndex GetSourceRegisterIndex()
		{
			return 0;
		}

		NcRegisterIndex GetDestinationRegisterIndex()
		{
			return 1;
		}
	};

	class NcByteCode
	{
	public:

	};

	class NcStackFrame
	{
	public:
		NcInstruction* GetNextInstruction()
		{
			return nullptr;
		}

		Variant& GetRegister(NcRegisterIndex index)
		{
			static Variant reg(0.0f);
			return reg;
		}

	private:

	};

	class NcVirtualMachine
	{
	public:
		void ExecuteScript(NcByteCode* code)
		{
			NcStackFrame* stack = PushStackFrame(code);
			while (stack != nullptr)	// Keep executing until the top function returns (we run out of stack frames)
			{
				NcInstruction* instruction = stack->GetNextInstruction();
				switch (instruction->GetOperation())
				{
				case NcOperation::AssignToRegister:
					{
						Variant& value = instruction->GetImmidiateValue();
						NcRegisterIndex registerIndex = instruction->GetDestinationRegisterIndex();
						Variant& destinationRegister = stack->GetRegister(registerIndex);

						destinationRegister = value;
						break;
					}
				case NcOperation::AddRegisters:
					{
						NcRegisterIndex indexA = instruction->GetDestinationRegisterIndex();
						NcRegisterIndex indexB = instruction->GetSourceRegisterIndex();

						Variant& registerA = stack->GetRegister(indexA);
						Variant& registerB = stack->GetRegister(indexB);

						registerA += registerB;
						break;
					}
				case NcOperation::SubtractRegisters:
					{
						NcRegisterIndex indexA = instruction->GetDestinationRegisterIndex();
						NcRegisterIndex indexB = instruction->GetSourceRegisterIndex();

						Variant& registerA = stack->GetRegister(indexA);
						Variant& registerB = stack->GetRegister(indexB);

						registerA -= registerB;
						break;
					}
				case NcOperation::MultiplyRegisters:
					{
						NcRegisterIndex indexA = instruction->GetDestinationRegisterIndex();
						NcRegisterIndex indexB = instruction->GetSourceRegisterIndex();

						Variant& registerA = stack->GetRegister(indexA);
						Variant& registerB = stack->GetRegister(indexB);

						registerA *= registerB;
						break;
					}
				case NcOperation::DivideRegisters:
					{
						NcRegisterIndex indexA = instruction->GetDestinationRegisterIndex();
						NcRegisterIndex indexB = instruction->GetSourceRegisterIndex();

						Variant& registerA = stack->GetRegister(indexA);
						Variant& registerB = stack->GetRegister(indexB);

						registerA /= registerB;
						break;
					}
				case NcOperation::Return:
					{
						stack = PopStackFrame();
						break;
					}
                default:
                        break;
				}
			}
		}

	private:
		NcStackFrame* PushStackFrame(NcByteCode* code)
		{
			return nullptr;
		}

		NcStackFrame* PopStackFrame()
		{
			return nullptr;
		}

		NcByteCode* LookUpByteCode(Name name)
		{
			return nullptr;
		}
	};
}
