#include "pch.h"
#include "textParser.h"

TextParser::TextParser(InputStream& stream)
	: m_stream(stream)
{

}

bool TextParser::ParseFloat(float& x)
{
	auto position = m_stream.tellg();
	if (m_stream >> x)
		return true;

	m_stream.seekg(position);
	return false;
}

bool TextParser::ParseFloat2(float& x, float& y)
{
	auto position = m_stream.tellg();
	if (m_stream >> x >> y)
		return true;

	m_stream.seekg(position);
	return false;
}

bool TextParser::ParseFloat3(float& x, float& y, float& z)
{
	auto position = m_stream.tellg();
	if (m_stream >> x >> y >> z)
		return true;

	m_stream.seekg(position);
	return false;
}

bool TextParser::ParseFloat4(float& x, float& y, float& z, float& w)
{
	auto position = m_stream.tellg();
	if (m_stream >> x >> y >> z >> w)
		return true;

	m_stream.seekg(position);
	return false;
}

bool TextParser::ParseInt(int32& x)
{
	auto position = m_stream.tellg();
	if (m_stream >> x)
		return true;

	m_stream.seekg(position);
	return false;
}

bool TextParser::ParseInt2(int32& x, int32& y)
{
	auto position = m_stream.tellg();
	if (m_stream >> x >> y)
		return true;

	m_stream.seekg(position);
	return false;
}

bool TextParser::ParseInt3(int32& x, int32& y, int32& z)
{
	auto position = m_stream.tellg();
	if (m_stream >> x >> y >> z)
		return true;

	m_stream.seekg(position);
	return false;
}

bool TextParser::ParseInt4(int32& x, int32& y, int32& z, int32& w)
{
	auto position = m_stream.tellg();
	if (m_stream >> x >> y >> z >> w)
		return true;

	m_stream.seekg(position);
	return false;
}

bool TextParser::ParseKeyword(const String& keyword)
{
	auto position = m_stream.tellg();
	for (char chr : keyword)
	{
		char streamChr = 0;
		if (m_stream >> streamChr)
		{
			if (streamChr != chr)
			{
				m_stream.seekg(position);
				return false;
			}
		}
	}

	return true;
}

bool TextParser::ParseIdentifier(String& identifier)
{
	identifier.clear();
	char chr;

	while (IsWhitespace(m_stream.peek()))
	{
		m_stream.read(&chr, 1);
	}

	if (IsAlpha(m_stream.peek()))
	{
		m_stream.read(&chr, 1);
		identifier += chr;

		while (IsAlphaNumeric(m_stream.peek()))
		{
			m_stream.read(&chr, 1);
			identifier += chr;
		}
	}

	return identifier.empty() == false;
}

bool TextParser::ParseText(String& text)
{
	text.clear();
	char chr;

	while (IsWhitespace(m_stream.peek()))
	{
		m_stream.read(&chr, 1);
	}

	if (IsQuotation(m_stream.peek()))
	{
		m_stream.read(&chr, 1);

		while (IsQuotation(m_stream.peek()) == false)
		{
			m_stream.read(&chr, 1);

			if (m_stream.eof())
				break;

			text += chr;
		}

		if (IsQuotation(m_stream.peek()))
			m_stream.read(&chr, 1);
	}
	else
	{
		while (IsWhitespace(m_stream.peek()) == false)
		{
			m_stream.read(&chr, 1);

			if (m_stream.eof() || chr == 0)
				break;

			text += chr;
		}
	}

	return text.empty() == false;
}

bool TextParser::ParseNonWhitespaceCharacter( char& chr )
{
	while (IsWhitespace(m_stream.peek()))
	{
		m_stream.read(&chr, 1);
	}
	
	m_stream.read(&chr, 1);
	
	return !m_stream.eof();
}

bool TextParser::IsParsing() const
{
	return !m_stream.eof();
}

bool TextParser::IsDigit(char chr)
{
	return '0' <= chr && chr <= '9';
}

bool TextParser::IsAlpha(char chr)
{
	return ('a' <= chr && chr <= 'z') || ('A' <= chr && chr <= 'Z') || chr == '_';
}

bool TextParser::IsAlphaNumeric(char chr)
{
	return IsAlpha(chr) || IsDigit(chr);
}

bool TextParser::IsWhitespace(char chr)
{
	return chr == ' ' || chr == '\n' || chr == '\r' || chr == '\t';
}

bool TextParser::IsQuotation(char chr)
{
	return chr == '\"' || chr == '\'';
}
