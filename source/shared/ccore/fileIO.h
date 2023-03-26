/* Copyright (c) 2016-2023 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "ccore/types.h"
#include "api.h"

typedef struct fc_file_t fc_file_t;
typedef struct fc_depot_t fc_depot_t;
typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;

typedef enum fc_depot_type_t
{
	FC_DEPOT_LOOSE_FILES = 0,
	FC_DEPOT_PACKAGE,
} fc_depot_type_t;

// depot is a broker between app and storage
typedef struct fc_depot_desc_t
{
	// path to directory for loose files or package file
	const char* path;

	// is it a loose files depot or packaged depot
	fc_depot_type_t type;
} fc_depot_desc_t;

// open a depot
CCORE_API fc_depot_t* fc_depot_mount(const fc_depot_desc_t* desc, fc_alloc_callbacks_t* pAllocCallbacks);

// close a depot
CCORE_API void fc_depot_unmount(fc_depot_t* depot, fc_alloc_callbacks_t* pAllocCallbacks);

// create a path hash - this shouldn't be done in packaged game
typedef u64 fc_file_path_t;
CCORE_API fc_file_path_t fc_file_path_create(fc_depot_t* depot, const char* path);

// hash to debug string
CCORE_API const char* fc_file_path_debug_str(const fc_depot_t* depot, fc_file_path_t path);

// helper for building paths
CCORE_API void fc_path_concat(char* output, const char* folderAbsolute, const char* directoryRelative, const char* fileName, const char* fileExtension);

// file API
#define FUR_SEEK_CUR 1
#define FUR_SEEK_END 2
#define FUR_SEEK_SET 0

CCORE_API fc_file_t* fc_file_open(fc_depot_t* depot, fc_file_path_t path, const char* mode);
CCORE_API void fc_file_close(fc_file_t* file);
CCORE_API void fc_file_seek(fc_file_t* file, i64 offset, i32 origin);
CCORE_API u64 fc_file_size(fc_file_t* file);
CCORE_API void fc_file_read(void* output, u64 size, u64 count, fc_file_t* file);
CCORE_API void fc_file_write(const void* input, u64 size, u64 count, fc_file_t* file);

// todo: implement async io

#ifdef __cplusplus
}
#endif // __cplusplus
