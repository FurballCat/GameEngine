/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "ccore/types.h"
#include "api.h"
	
typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;
	
typedef struct fm_xform fm_xform;
typedef struct fm_mat4 fm_mat4;
	
typedef u32 fc_string_hash_t;
	
typedef enum fi_result_t
{
	FI_RESULT_OK = 0,
	FI_RESULT_CANT_FIND_FILE = 1,
	FI_RESULT_UNKNOWN_IMPORT_ERROR,
	FI_RESULT_UNKNOWN_FILE_FORMAT_IMPORT_ERROR,
} fi_result_t;

typedef struct fi_depot_t
{
	const char* path;
} fi_depot_t;
	
void fc_path_concat(char* output, const char* folderAbsolute, const char* directoryRelative, const char* fileName, const char* fileExtension);

// -----
	
typedef struct fa_rig_t fa_rig_t;

typedef struct fi_import_rig_ctx_t
{
	const char* path;
} fi_import_rig_ctx_t;
	
CIMPORT_API fi_result_t fi_import_rig(const fi_depot_t* depot, const fi_import_rig_ctx_t* ctx, fa_rig_t** ppRig, fc_alloc_callbacks_t* pAllocCallbacks);
	
typedef struct fa_anim_clip_t fa_anim_clip_t;
	
typedef struct fi_import_anim_clip_ctx_t
{
	const char* path;
	bool extractRootMotion;
	
	const fa_rig_t* rig;
} fi_import_anim_clip_ctx_t;

CIMPORT_API fi_result_t fi_import_anim_clip(const fi_depot_t* depot, const fi_import_anim_clip_ctx_t* ctx, fa_anim_clip_t** ppAnimClip, fc_alloc_callbacks_t* pAllocCallbacks);
	
// -----
	
typedef struct fi_import_mesh_ctx_t
{
	const char* path;
} fi_import_mesh_ctx_t;

// if you want to change it then shaders and vertex layout descriptors needs to be changed
#define FUR_MAX_SKIN_INDICES_PER_VERTEX 4

typedef struct fr_resource_mesh_chunk_skin_t
{
	int16_t indices[FUR_MAX_SKIN_INDICES_PER_VERTEX];
	f32 weights[FUR_MAX_SKIN_INDICES_PER_VERTEX];
} fr_resource_mesh_chunk_skin_t;
	
typedef struct fr_resource_mesh_chunk_t
{
	f32* dataVertices;
	u32 numVertices;
	
	u32* dataIndices;
	u32 numIndices;
	
	u32 vertexStride;
	
	// skinning indices and weights, size of numVertices
	fr_resource_mesh_chunk_skin_t* dataSkinning;
	
	// bind pose
	fc_string_hash_t* boneNameHashes;
	fm_mat4* bindPose;
	u32 numBones;
	
} fr_resource_mesh_chunk_t;
	
typedef struct fr_resource_mesh_t
{
	u8 version;
	
	fr_resource_mesh_chunk_t* chunks;
	u32 numChunks;
} fr_resource_mesh_t;
	
CIMPORT_API fi_result_t fi_import_mesh(const fi_depot_t* depot, const fi_import_mesh_ctx_t* ctx, fr_resource_mesh_t** ppMesh, fc_alloc_callbacks_t* pAllocCallbacks);
	
CIMPORT_API void fr_mesh_release(fr_resource_mesh_t* ppMesh, fc_alloc_callbacks_t* pAllocCallbacks);

typedef struct fc_serializer_t fc_serializer_t;
CIMPORT_API void fr_resource_mesh_serialize(fc_serializer_t* pSerializer, fr_resource_mesh_t* mesh, fc_alloc_callbacks_t* pAllocCallbacks);

#ifdef __cplusplus
}
#endif // __cplusplus
