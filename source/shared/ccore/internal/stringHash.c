/* Copyright (c) 2016-2020 Furball Cat */

#include "stringHash.h"
#include <string.h>
#include "furAssert.h"
#include "memory.h"

/* magic numbers from http://www.isthe.com/chongo/tech/comp/fnv/ */
static const uint32_t InitialFNV = 2166136261U;
static const uint32_t FNVMultiple = 16777619U;

fc_string_hash_t fc_make_string_hash(const char* name)
{
	fc_string_hash_t result = InitialFNV;
	const size_t len = strlen(name);
	
	for(size_t i=0; i<len; ++i)
	{
		result = (result ^ name[i]) * FNVMultiple;
	}
	
	return result;
}

#define FUR_STRING_HASH_MAX_DEBUG_HASH_NAMES 512
#define FUR_STRING_HASH_BUFFER_CAPACITY FUR_STRING_HASH_MAX_DEBUG_HASH_NAMES * 128

typedef struct fc_string_hash_register_t
{
	fc_string_hash_t* hashes;
	const char** names;
	uint32_t namesCapacity;
	uint32_t namesCount;
	
	char* buffer;
	uint32_t bufferCapacity;
	uint32_t bufferOffset;
} fc_string_hash_register_t;

fc_string_hash_register_t g_hashRegister;

fc_string_hash_t fc_make_string_hash_and_register(const char* name)
{
	fc_string_hash_t hash = fc_make_string_hash(name);
	
	FUR_ASSERT(g_hashRegister.buffer);
	
	// check if this name is registered
	bool isRegistered = false;
	for(uint32_t i=0; i<g_hashRegister.namesCount; ++i)
	{
		if(g_hashRegister.hashes[i] == hash)
		{
			isRegistered = true;
			break;
		}
	}
	
	if(!isRegistered)
	{
		const uint32_t nameLen = (uint32_t)strlen(name);
		
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

const char* fc_string_hash_as_cstr_debug(fc_string_hash_t hash)
{
	FUR_ASSERT(g_hashRegister.buffer);
	
	for(uint32_t i=0; i<g_hashRegister.namesCount; ++i)
	{
		if(g_hashRegister.hashes[i] == hash)
		{
			return g_hashRegister.names[i];
		}
	}
	
	return "<unknown>";
}

void fc_string_hash_register_init(fc_alloc_callbacks_t* pAllocCallbacks)
{
	g_hashRegister.buffer = FUR_ALLOC_AND_ZERO(FUR_STRING_HASH_BUFFER_CAPACITY, 0, FC_MEMORY_SCOPE_DEBUG, pAllocCallbacks);
	g_hashRegister.bufferCapacity = FUR_STRING_HASH_BUFFER_CAPACITY;
	g_hashRegister.bufferOffset = 0;
	
	g_hashRegister.hashes = FUR_ALLOC_ARRAY_AND_ZERO(fc_string_hash_t, FUR_STRING_HASH_MAX_DEBUG_HASH_NAMES, 0, FC_MEMORY_SCOPE_DEBUG, pAllocCallbacks);
	g_hashRegister.names = FUR_ALLOC_ARRAY_AND_ZERO(const char*, FUR_STRING_HASH_MAX_DEBUG_HASH_NAMES, 0, FC_MEMORY_SCOPE_DEBUG, pAllocCallbacks);
	g_hashRegister.namesCapacity = FUR_STRING_HASH_MAX_DEBUG_HASH_NAMES;
	g_hashRegister.namesCount = 0;
}

void fc_string_hash_register_release(fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(g_hashRegister.buffer, pAllocCallbacks);
	FUR_FREE(g_hashRegister.hashes, pAllocCallbacks);
	FUR_FREE(g_hashRegister.names, pAllocCallbacks);
}
