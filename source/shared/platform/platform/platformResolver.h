#pragma once

namespace platform
{
	enum class PlatformType
	{
		Windows,
		Macintosh,
		Linux,
		Unknown
	};

	PLATFORM_API PlatformType GetCurrentPLatformType();
}