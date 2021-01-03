/* Copyright (c) 2020-2021 Furball Cat */

#include "textParsing.h"
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

bool fc_text_stream_is_eof(fc_text_stream_ro_t* stream, uint32_t offset)
{
	return stream->ptr + offset >= stream->end;
}

bool fc_text_parse_is_numeric(char chr)
{
	return isdigit(chr) != 0;
}

bool fc_text_parse_is_whitespace(char chr)
{
	return chr == ' ' || chr == '\r' || chr == '\t' || chr == '\n';
}

bool fc_text_parse_whitespaces(fc_text_stream_ro_t* stream)
{
	uint32_t i = 0;
	while(!fc_text_stream_is_eof(stream, i) && fc_text_parse_is_whitespace(stream->ptr[i]))
	{
		++i;
	}
	
	stream->ptr += i;
	
	return i;
}

bool fc_text_parse_character(fc_text_stream_ro_t* stream, char* outChar)
{
	// note: do not skip whitespaces here
	
	if(fc_text_stream_is_eof(stream, 0))
		return false;
	
	*outChar = stream->ptr[0];
	stream->ptr += 1;
	
	return true;
}

bool fc_text_parse_keyword(fc_text_stream_ro_t* stream, const char* keyword)
{
	fc_text_parse_whitespaces(stream);
	
	const uint32_t length = (uint32_t)strlen(keyword);
	for(uint32_t i=0; i<length; ++i)
	{
		if(fc_text_stream_is_eof(stream, i))
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

bool fc_text_parse_int32(fc_text_stream_ro_t* stream, int32_t* out)
{
	fc_text_parse_whitespaces(stream);
	
	char buf[64] = {};
	
	uint32_t i = 0;
	while(!fc_text_stream_is_eof(stream, i) && fc_text_parse_is_numeric(stream->ptr[i]) && i < 63)
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

bool fc_text_parse_float(fc_text_stream_ro_t* stream, float* out)
{
	fc_text_parse_whitespaces(stream);
	
	char buf[64] = {};
	
	uint32_t i = 0;
	while(!fc_text_stream_is_eof(stream, i) && (fc_text_parse_is_numeric(stream->ptr[i]) || stream->ptr[i] == '.') && i < 63)
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

bool fc_text_parse_skip_line(fc_text_stream_ro_t* stream)
{
	uint32_t i = 0;
	while(!fc_text_stream_is_eof(stream, i) && stream->ptr[i] != '\n')
	{
		++i;
	}
	
	if(!fc_text_stream_is_eof(stream, i) && stream->ptr[i] == '\n')
	{
		++i;
	}
	
	stream->ptr += i;
	
	return i > 0;
}
