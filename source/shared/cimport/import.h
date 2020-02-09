/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include <inttypes.h>
#include "api.h"
	
typedef enum fi_result_t
{
	FI_RESULT_OK = 0,
	FI_RESULT_CANT_FIND_FILE = 1,
	FI_RESULT_UNKNOWN_IMPORT_ERROR = 2,
} fi_result_t;

typedef struct fi_depot_t
{
	const char* path;
} fi_depot_t;
	
typedef struct fi_import_mesh_ctx_t
{
	const char* path;
} fi_import_mesh_ctx_t;

typedef struct fr_resource_mesh_chunk_t
{
	uint32_t numVertices;
	uint32_t numIndices;
} fr_resource_mesh_chunk_t;
	
typedef struct fr_resource_mesh_t
{
	uint8_t version;
} fr_resource_mesh_t;
	
CIMPORT_API fi_result_t fi_import_mesh(const fi_depot_t* depot, const fi_import_mesh_ctx_t* ctx, fr_resource_mesh_t* mesh);
	
#ifdef __cplusplus
}
#endif // __cplusplus
