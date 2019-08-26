#include "pch.h"
#include "platformResolver.h"

using namespace platform;

PLATFORM_API PlatformType  platform::GetCurrentPLatformType()
{
#ifdef PLATFORM_OSX
	return PlatformType::Macintosh;
#elif PLATFORM_WINDOWS
	return PlatformType::Windows;
#endif
	return PlatformType::Unknown;
}
