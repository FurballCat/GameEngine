/* Copyright (c) 2016-2019 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include "ccore/types.h"
#include "api.h"
	
typedef struct FcAllocator FcAllocator;
	
typedef struct fm_xform fm_xform;
typedef struct fm_mat4 fm_mat4;
	
typedef u32 FcStringId;
	
typedef enum FcImportResult
{
	FI_RESULT_OK = 0,
	FI_RESULT_CANT_FIND_FILE = 1,
	FI_RESULT_UNKNOWN_IMPORT_ERROR,
	FI_RESULT_UNKNOWN_FILE_FORMAT_IMPORT_ERROR,
} FcImportResult;

typedef struct fi_depot_t
{
	const char* path;
} fi_depot_t;

// -----
	
typedef struct FcRig FcRig;

typedef struct fi_import_rig_ctx_t
{
	const char* path;
} fi_import_rig_ctx_t;
	
CIMPORT_API FcImportResult fcImportRig(const fi_depot_t* depot, const fi_import_rig_ctx_t* ctx, FcRig** ppRig, FcAllocator* allocator);
	
typedef struct FcAnimClip FcAnimClip;
	
typedef struct FcImportAnimClipCtx
{
	const char* path;
	bool extractRootMotion;
	
	const FcRig* rig;
} FcImportAnimClipCtx;

CIMPORT_API FcImportResult fcImportAnimClip(const fi_depot_t* depot, const FcImportAnimClipCtx* ctx, FcAnimClip** ppAnimClip, FcAllocator* allocator);
	
// -----
	
typedef struct FcImportMeshCtx
{
	const char* path;
} FcImportMeshCtx;

// if you want to change it then shaders and vertex layout descriptors needs to be changed
#define FUR_MAX_SKIN_INDICES_PER_VERTEX 4

typedef struct FcMeshResourceChunkSkin
{
	int16_t indices[FUR_MAX_SKIN_INDICES_PER_VERTEX];
	f32 weights[FUR_MAX_SKIN_INDICES_PER_VERTEX];
} FcMeshResourceChunkSkin;
	
typedef struct FcMeshResourceChunk
{
	f32* dataVertices;
	u32 numVertices;
	
	u32* dataIndices;
	u32 numIndices;
	
	u32 vertexStride;
	
	// skinning indices and weights, size of numVertices
	FcMeshResourceChunkSkin* dataSkinning;
	
	// bind pose
	FcStringId* boneNameHashes;
	fm_mat4* bindPose;
	u32 numBones;
	
} FcMeshResourceChunk;
	
typedef struct FcMeshResource
{
	u8 version;
	
	FcMeshResourceChunk* chunks;
	u32 numChunks;
} FcMeshResource;
	
CIMPORT_API FcImportResult fcImportMeshResource(const fi_depot_t* depot, const FcImportMeshCtx* ctx, FcMeshResource** ppMesh, FcAllocator* allocator);
	
CIMPORT_API void fcMeshResourceRelease(FcMeshResource* ppMesh, FcAllocator* allocator);

typedef struct FcSerializer FcSerializer;
CIMPORT_API void fcMeshResourceSerialize(FcSerializer* pSerializer, FcMeshResource* mesh, FcAllocator* allocator);

#ifdef __cplusplus
}
#endif // __cplusplus
