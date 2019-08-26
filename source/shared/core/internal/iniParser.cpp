#include "pch.h"
#include "iniParser.h"
#include "textParser.h"

using namespace INIParser;

void INIParser::Parse(InputStream& stream, FuncOnSection onSection, FuncOnEntry onEntry)
{
	TextParser parser(stream);

	while (stream.eof() == false)
	{
		String key;
		String value;

		if (parser.ParseKeyword("[") && parser.ParseIdentifier(key) && parser.ParseKeyword("]"))
		{
			onSection(std::move(key));
		}
		else if (parser.ParseIdentifier(key) && parser.ParseKeyword("=") && parser.ParseText(value))
		{
			onEntry(std::move(key), std::move(value));
		}
		else
		{
			break;
		}
	}
}
