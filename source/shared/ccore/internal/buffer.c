/* Copyright (c) 2016-2021 Furball Cat */

#include "buffer.h"
#include <string.h>
#include <stdio.h>
#include "ccore/fileIO.h"

#include "memory.h"

bool fc_load_binary_file_into_binary_buffer(fc_depot_t* depot, fc_file_path_t path, fc_binary_buffer_t* pBuffer, fc_alloc_callbacks_t* pAllocCallbacks)
{
	fc_file_t* file = fc_file_open(depot, path, "rb");
	if(file && pBuffer)
	{
		const u64 size = fc_file_size(file);
		
		pBuffer->pData = FUR_ALLOC(size, 8, FC_MEMORY_SCOPE_GLOBAL, pAllocCallbacks);
		pBuffer->size = size;
		
		fc_file_read(pBuffer->pData, size, 1, file);
		fc_file_close(file);
		
		return true;
	}
	
	return false;
}

void fc_release_binary_buffer(fc_binary_buffer_t* pBuffer, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(pBuffer->pData, pAllocCallbacks);
}

void fc_init_binary_buffer_stream(const fc_binary_buffer_t* buffer, fc_binary_buffer_stream_t* outStream)
{
	outStream->buffer = buffer;
	outStream->pos = buffer->pData;
	outStream->endPos = (u8*)buffer->pData + buffer->size;
}

u32 fc_read_binary_buffer(fc_binary_buffer_stream_t* stream, u32 numBytes, void* output)
{
	if((u8*)stream->pos + numBytes <= (u8*)stream->endPos)
	{
		if(output != NULL)
		{
			memcpy(output, stream->pos, numBytes);
		}
		stream->pos = ((u8*)stream->pos) + numBytes;
		return numBytes;
	}
	
	return 0;
}

u32 fc_peek_binary_buffer(fc_binary_buffer_stream_t* stream, u32 numBytes, void* output)
{
	if((u8*)stream->pos + numBytes <= (u8*)stream->endPos)
	{
		if(output != NULL)
		{
			memcpy(output, stream->pos, numBytes);
		}
		return numBytes;
	}
	
	return 0;
}

bool fc_load_text_file_into_text_buffer(fc_depot_t* depot, fc_file_path_t path, fc_text_buffer_t* pBuffer, fc_alloc_callbacks_t* pAllocCallbacks)
{
	fc_file_t* file = fc_file_open(depot, path, "r");
	if (file && pBuffer)
	{
		const u64 size = fc_file_size(file);
		
		pBuffer->pData = (char*)FUR_ALLOC(size, 8, FC_MEMORY_SCOPE_GLOBAL, pAllocCallbacks);
		pBuffer->size = size;
		
		fc_file_read(pBuffer->pData, size, 1, file);
		fc_file_close(file);
		
		return true;
	}
	
	return false;
}

void fc_release_text_buffer(fc_text_buffer_t* pBuffer, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(pBuffer->pData, pAllocCallbacks);
}
