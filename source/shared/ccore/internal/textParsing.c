/* Copyright (c) 2020-2021 Furball Cat */

#include "textParsing.h"
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

bool fcTextStreamIsEOF(FcTextStreamRO* stream, u32 offset)
{
	return stream->ptr + offset >= stream->end;
}

bool fcTextParseIsNumeric(char chr)
{
	return isdigit(chr) != 0;
}

bool fcTextParseIsWhitespace(char chr)
{
	return chr == ' ' || chr == '\r' || chr == '\t' || chr == '\n';
}

bool fcTextParseWhitespaces(FcTextStreamRO* stream)
{
	u32 i = 0;
	while(!fcTextStreamIsEOF(stream, i) && fcTextParseIsWhitespace(stream->ptr[i]))
	{
		++i;
	}
	
	stream->ptr += i;
	
	return i;
}

bool fcTextParseCharacter(FcTextStreamRO* stream, char* outChar)
{
	// note: do not skip whitespaces here
	
	if(fcTextStreamIsEOF(stream, 0))
		return false;
	
	*outChar = stream->ptr[0];
	stream->ptr += 1;
	
	return true;
}

bool fcTextParseKeyword(FcTextStreamRO* stream, const char* keyword)
{
	fcTextParseWhitespaces(stream);
	
	const u32 length = (u32)strlen(keyword);
	for(u32 i=0; i<length; ++i)
	{
		if(fcTextStreamIsEOF(stream, i))
		{
			return false;
		}
		
		if(stream->ptr[i] != keyword[i])
		{
			return false;
		}
	}
	
	stream->ptr += length;
	
	return true;
}

bool fcTextParseInt32(FcTextStreamRO* stream, i32* out)
{
	fcTextParseWhitespaces(stream);
	
	char buf[64] = {0};
	
	u32 i = 0;
	while(!fcTextStreamIsEOF(stream, i) && fcTextParseIsNumeric(stream->ptr[i]) && i < 63)
	{
		buf[i] = stream->ptr[i];
		++i;
	}
	
	if(i == 0)
		return false;
	
	buf[i] = '\0';
	
	*out = atoi(buf);
	
	stream->ptr += i;
	
	return true;
}

bool fcTextParseUint32(FcTextStreamRO* stream, u32* out)
{
	fcTextParseWhitespaces(stream);
	
	char buf[64] = {0};
	
	u32 i = 0;
	while(!fcTextStreamIsEOF(stream, i) && fcTextParseIsNumeric(stream->ptr[i]) && i < 63)
	{
		buf[i] = stream->ptr[i];
		++i;
	}
	
	if(i == 0)
		return false;
	
	buf[i] = '\0';
	
	*out = (u32)atol(buf);
	
	stream->ptr += i;
	
	return true;
}

bool fcTextParseFloat(FcTextStreamRO* stream, f32* out)
{
	fcTextParseWhitespaces(stream);
	
	char buf[64] = {0};
	
	u32 i = 0;
	while(!fcTextStreamIsEOF(stream, i) && (fcTextParseIsNumeric(stream->ptr[i]) || stream->ptr[i] == '.') && i < 63)
	{
		buf[i] = stream->ptr[i];
		++i;
	}
	
	if(i == 0)
		return false;
	
	buf[i] = '\0';
	
	*out = atof(buf);
	
	stream->ptr += i;
	
	return true;
}

bool fcTextParseSkipLine(FcTextStreamRO* stream)
{
	u32 i = 0;
	while(!fcTextStreamIsEOF(stream, i) && stream->ptr[i] != '\n')
	{
		++i;
	}
	
	if(!fcTextStreamIsEOF(stream, i) && stream->ptr[i] == '\n')
	{
		++i;
	}
	
	stream->ptr += i;
	
	return i > 0;
}
