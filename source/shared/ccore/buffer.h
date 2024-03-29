/* Copyright (c) 2016-2021 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus
	
#include <inttypes.h>
#include <stdbool.h>
#include "api.h"
	
typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;
	
typedef struct fc_binary_buffer_t
{
	void* pData;
	size_t size;
} fc_binary_buffer_t;

CCORE_API bool fc_load_binary_file_into_binary_buffer(const char* path, fc_binary_buffer_t* pBuffer, fc_alloc_callbacks_t* pAllocCallbacks);
CCORE_API void fc_release_binary_buffer(fc_binary_buffer_t* pBuffer, fc_alloc_callbacks_t* pAllocCallbacks);

typedef struct fc_binary_buffer_stream_t
{
	const fc_binary_buffer_t* buffer;
	void* pos;		// position in binary stream
	void* endPos;	// indicates end of buffer
} fc_binary_buffer_stream_t;

CCORE_API void fc_init_binary_buffer_stream(const fc_binary_buffer_t* buffer, fc_binary_buffer_stream_t* outStream);
CCORE_API uint32_t fc_read_binary_buffer(fc_binary_buffer_stream_t* stream, uint32_t numBytes, void* output);
CCORE_API uint32_t fc_peek_binary_buffer(fc_binary_buffer_stream_t* stream, uint32_t numBytes, void* output);

typedef struct fc_text_buffer_t
{
	char* pData;
	size_t size;
} fc_text_buffer_t;
	
bool fc_load_text_file_into_text_buffer(const char* path, fc_text_buffer_t* pBuffer, fc_alloc_callbacks_t* pAllocCallbacks);
void fc_release_text_buffer(fc_text_buffer_t* pBuffer, fc_alloc_callbacks_t* pAllocCallbacks);
	
#ifdef __cplusplus
}
#endif // __cplusplus
