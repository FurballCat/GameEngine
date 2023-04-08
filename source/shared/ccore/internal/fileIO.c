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

typedef struct FcDepot
{
	const char* directory;

	FcFilePath hashes[FUR_MAX_FILES];
	const char paths[FUR_MAX_FILES][FUR_MAX_PATH_LENGTH];
	u32 numPaths;

} FcDepot;

FcDepot* fcDepotMount(const FcDepotDesc* desc, const FcAllocator* allocator)
{
	FcDepot* depot = FUR_ALLOC_AND_ZERO(sizeof(FcDepot), 8, FC_MEMORY_SCOPE_CORE, allocator);

	depot->directory = desc->path;

	return depot;
}

i32 fcDepotFindFileIdx(FcDepot* depot, FcFilePath path)
{
	for (u32 i = 0; i < depot->numPaths; ++i)
	{
		if (depot->hashes[i] == path)
			return i;
	}

	return -1;
}

void fcDepotUnmount(FcDepot* depot, const FcAllocator* allocator)
{
	FUR_FREE(depot, allocator);
}

FcFilePath fcDepotGetFilePath(FcDepot* depot, const char* path)
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

const char* fcFilePathAsDebugCstr(const FcDepot* depot, FcFilePath path)
{
	i32 idx = fcDepotFindFileIdx(depot, path);

	if (idx == -1)
		return NULL;

	return depot->paths[idx];
}

void fcPathConcat(char* output, const char* folderAbsolute, const char* directoryRelative, const char* fileName, const char* fileExtension)
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

FcFile* fcFileOpen(FcDepot* depot, FcFilePath path, const char* mode)
{
	i32 idx = fcDepotFindFileIdx(depot, path);

	if (idx == -1)
		return NULL;
	
	const char fullPath[FUR_MAX_PATH_LENGTH] = { 0 };
	fcPathConcat(fullPath, "", depot->directory, depot->paths[idx], "");

	return fopen(fullPath, mode);
}

void fcFileClose(FcFile* file)
{
	fclose((FILE*)file);
}

void fcFileSeek(FcFile* file, i64 offset, i32 origin)
{
	FILE* pFile = (FILE*)file;
	fseek(pFile, offset, origin);
}

u64 fcFileSize(FcFile* file)
{
	FILE* pFile = (FILE*)file;
	
	const u64 cur = ftell(pFile);
	fseek(pFile, 0, SEEK_END);
	const u64 size = ftell(pFile);
	fseek(pFile, 0, cur);

	return size;
}

void fcFileRead(void* output, u64 size, u64 count, FcFile* file)
{
	fread(output, size, count, (FILE*)file);
}

CCORE_API void fcFileWrite(const void* input, u64 size, u64 count, FcFile* file)
{
	fwrite(input, size, count, (FILE*)file);
}
