#pragma once

namespace res
{
	class RESOURCES_API AbsolutePath
	{
	public:
		AbsolutePath(const char* path);
		explicit AbsolutePath(const String& path);
	};
}
