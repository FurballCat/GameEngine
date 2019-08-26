#pragma once

namespace INIParser
{
	using FuncOnSection = std::function<void( String )>;
	using FuncOnEntry = std::function<void( String, String )>;

	CORE_API void Parse( InputStream& stream, FuncOnSection onSection, FuncOnEntry onEntry );
};
