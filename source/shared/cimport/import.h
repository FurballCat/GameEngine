/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include <inttypes.h>
#include "api.h"
	
typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;
	
typedef struct fm_xform fm_xform;
typedef struct fm_mat4 fm_mat4;
	
typedef uint32_t fc_string_hash_t;
	
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
} fi_import_anim_clip_ctx_t;

CIMPORT_API fi_result_t fi_import_anim_clip(const fi_depot_t* depot, const fi_import_anim_clip_ctx_t* ctx, fa_anim_clip_t** ppAnimClip, fc_alloc_callbacks_t* pAllocCallbacks);
	
// -----
	
typedef struct fi_import_mesh_ctx_t
{
	const char* path;
} fi_import_mesh_ctx_t;

#define FUR_MAX_SKIN_INDICES_PER_VERTEX 8
	
typedef enum fr_vertex_attribute_t
	{
		FR_VertexAttribute_Position3,
		FR_VertexAttribute_Color3,
		FR_VertexAttribute_TexCoord2,
		FR_VertexAttribute_SkinIndices4,
		FR_VertexAttribute_SkinWeights4,
	} fr_vertex_attribute_t;
	
typedef struct fr_resource_mesh_chunk_skin_t
{
	int16_t indices[FUR_MAX_SKIN_INDICES_PER_VERTEX];
	float weights[FUR_MAX_SKIN_INDICES_PER_VERTEX];
} fr_resource_mesh_chunk_skin_t;
	
typedef struct fr_resource_mesh_chunk_t
{
	float* dataVertices;
	uint32_t numVertices;
	
	uint32_t* dataIndices;
	uint32_t numIndices;
	
	uint32_t vertexStride;
	
	fr_vertex_attribute_t* vertexAttributes;
	uint32_t numVertexAttributes;
	
	// skinning indices and weights, size of numVertices
	fr_resource_mesh_chunk_skin_t* vertexSkin;
	
	// bind pose
	fc_string_hash_t* boneNameHashes;
	fm_mat4* bindPose;
	uint32_t numBones;
	
} fr_resource_mesh_chunk_t;
	
typedef struct fr_resource_mesh_t
{
	uint8_t version;
	
	fr_resource_mesh_chunk_t* chunks;
	uint32_t numChunks;
} fr_resource_mesh_t;
	
CIMPORT_API fi_result_t fi_import_mesh(const fi_depot_t* depot, const fi_import_mesh_ctx_t* ctx, fr_resource_mesh_t** ppMesh, fc_alloc_callbacks_t* pAllocCallbacks);
	
CIMPORT_API void fr_release_mesh(fr_resource_mesh_t** ppMesh, fc_alloc_callbacks_t* pAllocCallbacks);
	
#ifdef __cplusplus
}
#endif // __cplusplus
