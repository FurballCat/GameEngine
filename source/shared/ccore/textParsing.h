/* Copyright (c) 2020-2021 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus
	
#include "types.h"
#include "api.h"
	
typedef struct fc_text_stream_ro_t
{
	const char* ptr;
	const char* end;
} fc_text_stream_ro_t;

CCORE_API bool fc_text_stream_is_eof(fc_text_stream_ro_t* stream, u32 offset);
CCORE_API bool fc_text_parse_is_numeric(char chr);
CCORE_API bool fc_text_parse_is_whitespace(char chr);
CCORE_API bool fc_text_parse_whitespaces(fc_text_stream_ro_t* stream);
CCORE_API bool fc_text_parse_character(fc_text_stream_ro_t* stream, char* outChar);
CCORE_API bool fc_text_parse_keyword(fc_text_stream_ro_t* stream, const char* keyword);
CCORE_API bool fc_text_parse_int32(fc_text_stream_ro_t* stream, i32* out);
CCORE_API bool fc_text_parse_uint32(fc_text_stream_ro_t* stream, u32* out);
CCORE_API bool fc_text_parse_float(fc_text_stream_ro_t* stream, f32* out);
CCORE_API bool fc_text_parse_skip_line(fc_text_stream_ro_t* stream);
	
#ifdef __cplusplus
}
#endif // __cplusplus
