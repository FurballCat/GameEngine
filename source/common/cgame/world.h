/* Copyright (c) Furball Cat */

#pragma once

#include "ccore/types.h"
#include "cmath/mathtypes.h"

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

typedef u32 FcStringId;
typedef struct FcAllocator FcAllocator;
typedef struct FcMemRelHeapPool FcMemRelHeapPool;
typedef struct FcCameraSystem FcCameraSystem;

typedef union FcSpawninfoPropValue
{
	i32 asInt32;
	f32 asFloat;
	FcStringId asStringId;
} FcSpawninfoPropValue;

typedef struct FcSpawnInfoProperties
{
	i32 num;
	FcStringId* names;
	FcSpawninfoPropValue* values;
} FcSpawnInfoProperties;

// info for initialising game object
typedef struct FcSpawnInfo
{
	// unique ID of the game object, can be used in scripts
	FcStringId gameObjectName;
	
	// key-value list of properties (only different than default)
	FcSpawnInfoProperties props;
	
} FcSpawnInfo;

// get key-value properties through these functions
f32 fcSpawnInfoGetFloat(const FcSpawnInfo* info, FcStringId name, f32 defaultValue);
i32 fcSpawnInfoGetInt(const FcSpawnInfo* info, FcStringId name, i32 defaultValue);
FcStringId fcSpawnInfoGetStringId(const FcSpawnInfo* info, FcStringId name, FcStringId defaultValue);

// stack allocator for game object used on init
typedef struct FcStackAllocPool
{
	void* ptr;
	i32 size;
	i32 capacity;
} FcStackAllocPool;

void* fcStackAlloc(FcStackAllocPool* allocator, i32 size);

typedef enum FcUpdateBucket
{
	FG_UPDATE_BUCKET_CHARACTERS = 0,
	FG_UPDATE_BUCKET_COUNT
} FcUpdateBucket;

typedef struct FcResourceRegister FcResourceRegister;
typedef struct FcSystems FcSystems;
typedef struct FcGameObject FcGameObject;
typedef struct FcWorld FcWorld;

typedef struct FcGameObjectInitCtx
{
	// all the information from spawner
	const FcSpawnInfo* info;
	
	// each object can allocate its memory on stack in level heap during init
	FcAllocator* stackAlloc;
	
	// register of all available resources
	const FcResourceRegister* resources;
	
	// all systems
	const FcSystems* systems;
	
	// current global time
	f64 globalTime;	// todo: remove?
} FcGameObjectInitCtx;

typedef bool (*FcGameObjectInitFn)(FcGameObject* gameObject, FcGameObjectInitCtx* ctx);

typedef struct FcGameObjectUpdateCtx
{
	f32 dt;
	FcWorld* world;
} FcGameObjectUpdateCtx;

typedef void (*FcGameObjectUpdateFn)(FcGameObject* gameObject, FcGameObjectUpdateCtx* ctx);

typedef FcVariant (*FcGameObjectGetVarFn)(FcGameObject* gameObject, FcStringId varName);
typedef void (*FcGameObjectSetVarFn)(FcGameObject* gameObject, FcStringId varName, FcVariant value);

typedef struct FcAnimActionArgs FcAnimActionArgs;
typedef struct FcAnimClip FcAnimClip;

typedef struct FcGameObjectAnimateCtx
{
	const FcAnimActionArgs* animArgs;
	const FcAnimClip* animClip;
	bool useLocomotion;
	bool forceLoop;
} FcGameObjectAnimateCtx;

typedef void (*FcGameObjectAnimateFn)(FcGameObject* gameObject, const FcGameObjectAnimateCtx* ctx);

typedef struct FcGameObjectFuncs
{
	FcGameObjectInitFn init;
	FcGameObjectUpdateFn update;
	FcGameObjectGetVarFn getVar;
	FcGameObjectSetVarFn setVar;
	FcGameObjectAnimateFn animate;
} FcGameObjectFuncs;

// handle for the game object data
typedef struct FcGameObject
{
	const FcGameObjectFuncs* fn;
	FcStringId name;
	fm_xform transform;

} FcGameObject;

typedef struct FcBinaryBuffer FcBinaryBuffer;
typedef struct FcAnimClip FcAnimClip;
typedef struct FcRig FcRig;
typedef struct FcRenderProxy FcRenderProxy;

FUR_DEFINE_MAP_TYPE(FcResourceMapScripts, FcStringId, const FcBinaryBuffer*);
FUR_DEFINE_MAP_TYPE(FcResourceMapAnimClips, FcStringId, const FcAnimClip*);
FUR_DEFINE_MAP_TYPE(FcResourceMapRigs, FcStringId, const FcRig*);
FUR_DEFINE_MAP_TYPE(FcResourceMapMeshes, FcStringId, const FcRenderProxy*);

typedef struct FcResourceRegister
{
	FcResourceMapScripts scripts;
	FcResourceMapAnimClips animations;
	FcResourceMapRigs rigs;
	FcResourceMapMeshes meshes;
} FcResourceRegister;

const FcAnimClip* fcResourceRegisterFindAnimClip(const FcResourceRegister* reg, FcStringId name);
const FcBinaryBuffer* fcResourceRegisterFindScript(const FcResourceRegister* reg, FcStringId name);
const FcRig* fcResourceRegisterFindRig(const FcResourceRegister* reg, FcStringId name);
const FcRenderProxy* fcResourceRegisterFindMesh(const FcResourceRegister* reg, FcStringId name);

void fcResourceRegisterAddAnimClip(FcResourceRegister* reg, FcStringId name, const FcAnimClip* res);
void fcResourceRegisterAddScript(FcResourceRegister* reg, FcStringId name, const FcBinaryBuffer* res);
void fcResourceRegisterAddRig(FcResourceRegister* reg, FcStringId name, const FcRig* res);
void fcResourceRegisterAddMesh(FcResourceRegister* reg, FcStringId name, const FcRenderProxy* res);

typedef struct FcRenderer FcRenderer;
typedef struct FcAnimSystem FcAnimSystem;
typedef struct FcInputActionSystem FcInputActionSystem;
typedef struct FcPhysics FcPhysics;

typedef struct FcSystems
{
	FcRenderer* renderer;
	FcAnimSystem* animation;
	FcCameraSystem* camera;
	FcInputActionSystem* inputAction;
	FcPhysics* physics;
} FcSystems;

// used for defining how to initialise specific type of game object
typedef struct FcGameObjectFactory
{
	// game object custom functions
	FcGameObjectFuncs fn;
	
	// size information for relocatable memory management, see level heap
	u32 memoryMaxSize;
	
	// which bucket is the game object updated in
	FcUpdateBucket updateBucket;
} FcGameObjectFactory;

void fcGameObjectFactoryRegisterNew(FcStringId typeName, FcGameObjectFactory factory);

// use index to get pointer to the game object, then compare go.name to name to f64 check
typedef struct FcGameObjectHandle
{
	// index of the game object slot
	i32 index;
	
	// unique name
	FcStringId name;
} FcGameObjectHandle;

FUR_DEFINE_ARRAY_TYPE(FcArrayGameObjectHandles, FcGameObjectHandle);
FUR_DEFINE_ARRAY_TYPE(FcArrayGameObjects, FcGameObject);

typedef struct FcSpawner
{
	FcStringId typeName;
	FcStringId name;
	FcSpawnInfo info;
} FcSpawner;

#define MAX_GAME_OBJECTS_SPAWNED 2048

typedef struct FcGameObjectStorage
{
	FcGameObject* ptr[MAX_GAME_OBJECTS_SPAWNED];
	const FcSpawner* spawner[MAX_GAME_OBJECTS_SPAWNED];
	
	// number of initialised game objects
	i32 numInit;
	
	// total number of game objects
	i32 num;
} FcGameObjectStorage;

typedef struct FcSpawnDesc
{
	const FcSpawner* spawner;
	fm_xform initialTransform;

} FcSpawnDesc;


typedef struct FcSpawnBatch
{
	FcSpawnDesc elems[MAX_GAME_OBJECTS_SPAWNED];
	i32 num;

} FcSpawnBatch;

typedef struct FcWorld
{
	// list of all game object info (use game object index to get data)
	FcGameObjectStorage gameObjects;
	
	// memory for dynamic objects (game objects), does not include resources like animation, meshes, textures
	FcMemRelHeapPool* levelHeap;
	
	// all available resources in the world
	FcResourceRegister resources;
	
	// low-level systems
	FcSystems systems;
	
	// game objects in update buckets
	FcArrayGameObjectHandles buckets[FG_UPDATE_BUCKET_COUNT];
	
	// pending spawn requests
	FcSpawnBatch spawnBatch;
	
} FcWorld;

typedef struct FcWorldCreateInfo
{
	FcSystems systems;
} FcWorldCreateInfo;

FcResult fcCreateWorld(FcWorldCreateInfo* desc, const FcAllocator* allocator, FcWorld** world);
void fcDestroyWorld(FcWorld* world, const FcAllocator* allocator);

typedef struct FcWorldUpdateCtx
{
	f32 dt;
} FcWorldUpdateCtx;

void fcUpdateSpawning(FcWorld* world);
void fcWorldUpdate(FcWorld* world, FcWorldUpdateCtx* ctx, FcUpdateBucket bucket);

void fcSpawn(const FcSpawnDesc* desc, FcWorld* world);

#ifdef __cplusplus
}
#endif // __cplusplus
