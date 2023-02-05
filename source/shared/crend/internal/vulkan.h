/* Copyright (c) Furball Cat */

#pragma once

// include Vulkan API headers depending on platform
#if PLATFORM_OSX
#include "vulkansdk/macOS/include/vulkan/vulkan.h"
#elif PLATFORM_WINDOWS
#include "vulkan/vulkan.h"
#else
	#error Unsupported platform for Vulkan API
#endif