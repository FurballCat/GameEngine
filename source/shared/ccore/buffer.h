/* Copyright (c) 2016-2021 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus
	
#include "types.h"
#include "api.h"
	
typedef struct FcAllocator FcAllocator;
typedef u64 FcFilePath;
typedef struct FcDepot FcDepot;
	
typedef struct FcBinaryBuffer
{
	void* pData;
	u64 size;
} FcBinaryBuffer;

CCORE_API bool fcBinaryBufferLoad(FcDepot* depot, FcFilePath path, FcBinaryBuffer* pBuffer, FcAllocator* allocator);
CCORE_API void fcBinaryBufferRelease(FcBinaryBuffer* pBuffer, FcAllocator* allocator);

typedef struct FcBinaryBufferStream
{
	const FcBinaryBuffer* buffer;
	void* pos;		// position in binary stream
	void* endPos;	// indicates end of buffer
} FcBinaryBufferStream;

CCORE_API void fcBinaryBufferStreamInit(const FcBinaryBuffer* buffer, FcBinaryBufferStream* outStream);
CCORE_API u32 fcBinaryBufferStreamRead(FcBinaryBufferStream* stream, u32 numBytes, void* output);
CCORE_API u32 fcBinaryBufferStreamPeek(FcBinaryBufferStream* stream, u32 numBytes, void* output);

typedef struct FcTextBuffer
{
	char* pData;
	u64 size;
} FcTextBuffer;
	
bool fcTextBufferLoad(FcDepot* depot, FcFilePath path, FcTextBuffer* pBuffer, FcAllocator* allocator);
void fcTextBufferRelease(FcTextBuffer* pBuffer, FcAllocator* allocator);
	
#ifdef __cplusplus
}
#endif // __cplusplus
