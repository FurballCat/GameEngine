#include "pch.h"
#include "windowsSimpleApp.h"
#include "profiler/profiler.h"
#include "gpu/public.h"
#include "windowsInputKeyboard.h"
#include "input/inputCollector.h"
#include "windowsHIDInput.h"

using namespace platform;

// Implement platform specific global interface
namespace platform
{
	PLATFORM_API int32 RunSimpleApp(const SimpleAppDesc& desc)
	{
		if (desc.app == nullptr)
			return 1;

		if (desc.app->Initialize() == false)
			return 2;

		// TODO

		desc.app->Shutdown();

		return 0;
	}
}