/* Copyright (c) 2016-2020 Furball Cat */

#include "stringHash.h"
#include <string.h>

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
