/* Copyright (c) 2016-2021 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus
	
#include "types.h"
#include "api.h"
	
typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;
	
typedef struct fc_binary_buffer_t
{
	void* pData;
	u64 size;
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
CCORE_API u32 fc_read_binary_buffer(fc_binary_buffer_stream_t* stream, u32 numBytes, void* output);
CCORE_API u32 fc_peek_binary_buffer(fc_binary_buffer_stream_t* stream, u32 numBytes, void* output);

typedef struct fc_text_buffer_t
{
	char* pData;
	u64 size;
} fc_text_buffer_t;
	
bool fc_load_text_file_into_text_buffer(const char* path, fc_text_buffer_t* pBuffer, fc_alloc_callbacks_t* pAllocCallbacks);
void fc_release_text_buffer(fc_text_buffer_t* pBuffer, fc_alloc_callbacks_t* pAllocCallbacks);
	
#ifdef __cplusplus
}
#endif // __cplusplus
