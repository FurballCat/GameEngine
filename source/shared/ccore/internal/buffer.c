/* Copyright (c) 2016-2021 Furball Cat */

#include "buffer.h"
#include <string.h>
#include <stdio.h>

#include "memory.h"

bool fc_load_binary_file_into_binary_buffer(const char* path, fc_binary_buffer_t* pBuffer, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FILE* pFile = fopen(path, "rb");
	if(pFile && pBuffer)
	{
		fseek(pFile, 0, SEEK_END);
		size_t size = ftell(pFile);
		fseek(pFile, 0, SEEK_SET);
		
		pBuffer->pData = FUR_ALLOC(size, 8, FC_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
		pBuffer->size = size;
		
		fread(pBuffer->pData, size, 1, pFile);
		fclose(pFile);
		
		return true;
	}
	
	return false;
}

void fc_release_binary_buffer(fc_binary_buffer_t* pBuffer, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(pBuffer->pData, pAllocCallbacks);
}

bool fc_load_text_file_into_text_buffer(const char* path, fc_text_buffer_t* pBuffer, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FILE* pFile = fopen(path, "r");
	if(pFile && pBuffer)
	{
		fseek(pFile, 0, SEEK_END);
		size_t size = ftell(pFile);
		fseek(pFile, 0, SEEK_SET);
		
		pBuffer->pData = (char*)FUR_ALLOC(size, 8, FC_MEMORY_SCOPE_DEFAULT, pAllocCallbacks);
		pBuffer->size = size;
		
		fread(pBuffer->pData, size, 1, pFile);
		fclose(pFile);
		
		return true;
	}
	
	return false;
}

void fc_release_text_buffer(fc_text_buffer_t* pBuffer, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(pBuffer->pData, pAllocCallbacks);
}
