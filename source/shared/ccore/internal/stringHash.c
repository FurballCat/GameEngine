/* Copyright (c) 2016-2020 Furball Cat */

#include "stringHash.h"
#include <string.h>
#include "furAssert.h"
#include "memory.h"

/* magic numbers from http://www.isthe.com/chongo/tech/comp/fnv/ */
static const u32 InitialFNV = 2166136261U;
static const u32 FNVMultiple = 16777619U;

FcStringId fcMakeStringId(const char* name)
{
	FcStringId result = InitialFNV;
	const u64 len = strlen(name);
	
	for(u64 i=0; i<len; ++i)
	{
		result = (result ^ name[i]) * FNVMultiple;
	}
	
	return result;
}

#define FUR_STRING_HASH_MAX_DEBUG_HASH_NAMES 512
#define FUR_STRING_HASH_BUFFER_CAPACITY FUR_STRING_HASH_MAX_DEBUG_HASH_NAMES * 128

typedef struct FcStringIdRegister
{
	FcStringId* hashes;
	const char** names;
	u32 namesCapacity;
	u32 namesCount;
	
	char* buffer;
	u32 bufferCapacity;
	u32 bufferOffset;
} FcStringIdRegister;

FcStringIdRegister g_hashRegister;

FcStringId fcMakeStringIdAndRegister(const char* name)
{
	FcStringId hash = fcMakeStringId(name);
	
	if(!g_hashRegister.buffer)
		return hash;
	
	// check if this name is registered
	bool isRegistered = false;
	for(u32 i=0; i<g_hashRegister.namesCount; ++i)
	{
		if(g_hashRegister.hashes[i] == hash)
		{
			isRegistered = true;
			break;
		}
	}
	
	if(!isRegistered)
	{
		const u32 nameLen = (u32)strlen(name);
		
		FUR_ASSERT(g_hashRegister.bufferOffset + nameLen + 1 < g_hashRegister.bufferCapacity);
		FUR_ASSERT(g_hashRegister.namesCount < g_hashRegister.namesCapacity);
		
		char* bufPtr = g_hashRegister.buffer + g_hashRegister.bufferOffset;
		memcpy(bufPtr, name, nameLen);
		bufPtr[nameLen] = '\0';
		g_hashRegister.bufferOffset += nameLen + 1;
		g_hashRegister.hashes[g_hashRegister.namesCount] = hash;
		g_hashRegister.names[g_hashRegister.namesCount] = bufPtr;
		g_hashRegister.namesCount += 1;
	}
	
	return hash;
}

const char* fcStringIdAsDebugCstr(FcStringId hash)
{
	FUR_ASSERT(g_hashRegister.buffer);
	
	for(u32 i=0; i<g_hashRegister.namesCount; ++i)
	{
		if(g_hashRegister.hashes[i] == hash)
		{
			return g_hashRegister.names[i];
		}
	}
	
	return "<unknown>";
}

void fcStringIdRegisterInit(const FcAllocator* allocator)
{
	g_hashRegister.buffer = FUR_ALLOC_AND_ZERO(FUR_STRING_HASH_BUFFER_CAPACITY, 0, FC_MEMORY_SCOPE_DEBUG, allocator);
	g_hashRegister.bufferCapacity = FUR_STRING_HASH_BUFFER_CAPACITY;
	g_hashRegister.bufferOffset = 0;
	
	g_hashRegister.hashes = FUR_ALLOC_ARRAY_AND_ZERO(FcStringId, FUR_STRING_HASH_MAX_DEBUG_HASH_NAMES, 0, FC_MEMORY_SCOPE_DEBUG, allocator);
	g_hashRegister.names = FUR_ALLOC_ARRAY_AND_ZERO(const char*, FUR_STRING_HASH_MAX_DEBUG_HASH_NAMES, 0, FC_MEMORY_SCOPE_DEBUG, allocator);
	g_hashRegister.namesCapacity = FUR_STRING_HASH_MAX_DEBUG_HASH_NAMES;
	g_hashRegister.namesCount = 0;
}

void fcStringIdRegisterRelease(const FcAllocator* allocator)
{
	FUR_FREE(g_hashRegister.buffer, allocator);
	FUR_FREE(g_hashRegister.hashes, allocator);
	FUR_FREE(g_hashRegister.names, allocator);
}
