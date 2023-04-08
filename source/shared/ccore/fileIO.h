/* Copyright (c) 2016-2023 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "ccore/types.h"
#include "api.h"

typedef struct FcFile FcFile;
typedef struct FcDepot FcDepot;
typedef struct FcAllocator FcAllocator;

typedef enum FcDepotType
{
	FC_DEPOT_LOOSE_FILES = 0,
	FC_DEPOT_PACKAGE,
} FcDepotType;

// depot is a broker between app and storage
typedef struct FcDepotDesc
{
	// path to directory for loose files or package file
	const char* path;

	// is it a loose files depot or packaged depot
	FcDepotType type;
} FcDepotDesc;

// open a depot
CCORE_API FcDepot* fcDepotMount(const FcDepotDesc* desc, const FcAllocator* allocator);

// close a depot
CCORE_API void fcDepotUnmount(FcDepot* depot, const FcAllocator* allocator);

// create a path hash - this shouldn't be done in packaged game
typedef u64 FcFilePath;
CCORE_API FcFilePath fcDepotGetFilePath(FcDepot* depot, const char* path);

// hash to debug string
CCORE_API const char* fcFilePathAsDebugCstr(const FcDepot* depot, FcFilePath path);

// helper for building paths
CCORE_API void fcPathConcat(char* output, const char* folderAbsolute, const char* directoryRelative, const char* fileName, const char* fileExtension);

// file API
#define FUR_SEEK_CUR 1
#define FUR_SEEK_END 2
#define FUR_SEEK_SET 0

CCORE_API FcFile* fcFileOpen(FcDepot* depot, FcFilePath path, const char* mode);
CCORE_API void fcFileClose(FcFile* file);
CCORE_API void fcFileSeek(FcFile* file, i64 offset, i32 origin);
CCORE_API u64 fcFileSize(FcFile* file);
CCORE_API void fcFileRead(void* output, u64 size, u64 count, FcFile* file);
CCORE_API void fcFileWrite(const void* input, u64 size, u64 count, FcFile* file);

// todo: implement async io

#ifdef __cplusplus
}
#endif // __cplusplus
