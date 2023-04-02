/* Copyright (c) 2020-2021 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus
	
#include "types.h"
#include "api.h"
	
typedef struct FcTextStreamRO
{
	const char* ptr;
	const char* end;
} FcTextStreamRO;

CCORE_API bool fcTextStreamIsEOF(FcTextStreamRO* stream, u32 offset);
CCORE_API bool fcTextParseIsNumeric(char chr);
CCORE_API bool fcTextParseIsWhitespace(char chr);
CCORE_API bool fcTextParseWhitespaces(FcTextStreamRO* stream);
CCORE_API bool fcTextParseCharacter(FcTextStreamRO* stream, char* outChar);
CCORE_API bool fcTextParseKeyword(FcTextStreamRO* stream, const char* keyword);
CCORE_API bool fcTextParseInt32(FcTextStreamRO* stream, i32* out);
CCORE_API bool fcTextParseUint32(FcTextStreamRO* stream, u32* out);
CCORE_API bool fcTextParseFloat(FcTextStreamRO* stream, f32* out);
CCORE_API bool fcTextParseSkipLine(FcTextStreamRO* stream);
	
#ifdef __cplusplus
}
#endif // __cplusplus
