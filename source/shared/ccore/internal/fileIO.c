/* Copyright (c) Furball Cat */

#include "fileIO.h"
#include <stdio.h>

#include "memory.h"
#include "furAssert.h"
#include "profiler.h"

#define FUR_MAX_FILES 4096
#define FUR_MAX_PATH_LENGTH 256

u64 murmur2(const void* key, int len)
{
	const u64 seed = 0x123456789abcdefull * 8;

	const u64 m = 0xc6a4a7935bd1e995ull;
	const i32 r = 47;

	u64 h = seed ^ (len * m);

	const u64* data = (const u64*)key;
	const u64* end = data + (len / 8);

	while (data != end)
	{
		u64 k = *data++;

		k *= m;
		k ^= k >> r;
		k *= m;

		h ^= k;
		h *= m;
	}

	const u8* data2 = (const u8*)data;

	switch (len & 7)
	{
	case 7: h ^= (u64)(data2[6]) << 48;
	case 6: h ^= (u64)(data2[5]) << 40;
	case 5: h ^= (u64)(data2[4]) << 32;
	case 4: h ^= (u64)(data2[3]) << 24;
	case 3: h ^= (u64)(data2[2]) << 16;
	case 2: h ^= (u64)(data2[1]) << 8;
	case 1: h ^= (u64)(data2[0]);
		h *= m;
	};

	h ^= h >> r;
	h *= m;
	h ^= h >> r;

	return h;
}

typedef struct fc_depot_t
{
	const char* directory;

	fc_file_path_t hashes[FUR_MAX_FILES];
	const char paths[FUR_MAX_FILES][FUR_MAX_PATH_LENGTH];
	u32 numPaths;

} fc_depot_t;

fc_depot_t* fc_depot_mount(const fc_depot_desc_t* desc, fc_alloc_callbacks_t* pAllocCallbacks)
{
	fc_depot_t* depot = FUR_ALLOC_AND_ZERO(sizeof(fc_depot_t), 8, FC_MEMORY_SCOPE_CORE, pAllocCallbacks);

	depot->directory = desc->path;

	return depot;
}

i32 fc_depot_find_file_idx(fc_depot_t* depot, fc_file_path_t path)
{
	for (u32 i = 0; i < depot->numPaths; ++i)
	{
		if (depot->hashes[i] == path)
			return i;
	}

	return -1;
}

void fc_depot_unmount(fc_depot_t* depot, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_FREE(depot, pAllocCallbacks);
}

fc_file_path_t fc_file_path_create(fc_depot_t* depot, const char* path)
{
	const u64 length = strlen(path);
	FUR_ASSERT(length+1 < FUR_MAX_PATH_LENGTH);

	u64 hash = murmur2(path, length);

	const u32 idx = depot->numPaths;
	FUR_ASSERT(idx < FUR_MAX_FILES);
	depot->numPaths++;

	memcpy(depot->paths[idx], path, length+1);
	depot->hashes[idx] = hash;

	return hash;
}

const char* fc_file_path_debug_str(const fc_depot_t* depot, fc_file_path_t path)
{
	i32 idx = fc_depot_find_file_idx(depot, path);

	if (idx == -1)
		return NULL;

	return depot->paths[idx];
}

void fc_path_concat(char* output, const char* folderAbsolute, const char* directoryRelative, const char* fileName, const char* fileExtension)
{
	const u64 folderLen = strlen(folderAbsolute);
	const u64 dirLen = strlen(directoryRelative);
	const u64 nameLen = strlen(fileName);
	const u64 extLen = strlen(fileExtension);

	memcpy(output, folderAbsolute, folderLen);
	memcpy(output + folderLen, directoryRelative, dirLen);
	memcpy(output + folderLen + dirLen, fileName, nameLen);
	memcpy(output + folderLen + dirLen + nameLen, fileExtension, extLen);
	memcpy(output + folderLen + dirLen + nameLen + extLen, "\0", 1);
}

fc_file_t* fc_file_open(fc_depot_t* depot, fc_file_path_t path, const char* mode)
{
	i32 idx = fc_depot_find_file_idx(depot, path);

	if (idx == -1)
		return NULL;
	
	const char fullPath[FUR_MAX_PATH_LENGTH] = { 0 };
	fc_path_concat(fullPath, "", depot->directory, depot->paths[idx], "");

	return fopen(fullPath, mode);
}

void fc_file_close(fc_file_t* file)
{
	fclose((FILE*)file);
}

void fc_file_seek(fc_file_t* file, i64 offset, i32 origin)
{
	FILE* pFile = (FILE*)file;
	fseek(pFile, offset, origin);
}

u64 fc_file_size(fc_file_t* file)
{
	FILE* pFile = (FILE*)file;
	
	const u64 cur = ftell(pFile);
	fseek(pFile, 0, SEEK_END);
	const u64 size = ftell(pFile);
	fseek(pFile, 0, cur);

	return size;
}

void fc_file_read(void* output, u64 size, u64 count, fc_file_t* file)
{
	fread(output, size, count, (FILE*)file);
}

CCORE_API void fc_file_write(const void* input, u64 size, u64 count, fc_file_t* file)
{
	fwrite(input, size, count, (FILE*)file);
}
