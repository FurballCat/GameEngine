/* Copyright (c) 2016-2021 Furball Cat */

#include "buffer.h"
#include <string.h>
#include <stdio.h>
#include "ccore/fileIO.h"

#include "memory.h"

bool fcBinaryBufferLoad(FcDepot* depot, FcFilePath path, FcBinaryBuffer* pBuffer, FcAllocator* pAllocCallbacks)
{
	FcFile* file = fcFileOpen(depot, path, "rb");
	if(file && pBuffer)
	{
		const u64 size = fcFileSize(file);
		
		pBuffer->pData = FUR_ALLOC(size, 8, FC_MEMORY_SCOPE_GLOBAL, pAllocCallbacks);
		pBuffer->size = size;
		
		fcFileRead(pBuffer->pData, size, 1, file);
		fcFileClose(file);
		
		return true;
	}
	
	return false;
}

void fcBinaryBufferRelease(FcBinaryBuffer* pBuffer, FcAllocator* pAllocCallbacks)
{
	FUR_FREE(pBuffer->pData, pAllocCallbacks);
}

void fcBinaryBufferStreamInit(const FcBinaryBuffer* buffer, FcBinaryBufferStream* outStream)
{
	outStream->buffer = buffer;
	outStream->pos = buffer->pData;
	outStream->endPos = (u8*)buffer->pData + buffer->size;
}

u32 fcBinaryBufferStreamRead(FcBinaryBufferStream* stream, u32 numBytes, void* output)
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

u32 fcBinaryBufferStreamPeek(FcBinaryBufferStream* stream, u32 numBytes, void* output)
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

bool fcTextBufferLoad(FcDepot* depot, FcFilePath path, FcTextBuffer* pBuffer, FcAllocator* pAllocCallbacks)
{
	FcFile* file = fcFileOpen(depot, path, "r");
	if (file && pBuffer)
	{
		const u64 size = fcFileSize(file);
		
		pBuffer->pData = (char*)FUR_ALLOC(size, 8, FC_MEMORY_SCOPE_GLOBAL, pAllocCallbacks);
		pBuffer->size = size;
		
		fcFileRead(pBuffer->pData, size, 1, file);
		fcFileClose(file);
		
		return true;
	}
	
	return false;
}

void fcTextBufferRelease(FcTextBuffer* pBuffer, FcAllocator* pAllocCallbacks)
{
	FUR_FREE(pBuffer->pData, pAllocCallbacks);
}
