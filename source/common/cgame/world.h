/* Copyright (c) Furball Cat */

#pragma once

#include "ccore/types.h"

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

typedef u32 fc_string_hash_t;
typedef struct fc_alloc_callbacks_t fc_alloc_callbacks_t;
typedef struct fc_mem_rel_heap_alloc_t fc_mem_rel_heap_alloc_t;

typedef union fg_spawn_info_prop_value_t
{
	i32 asInt32;
	f32 asFloat;
	fc_string_hash_t asStringHash;
} fg_spawn_info_prop_value_t;

typedef struct fg_spawn_info_properties_t
{
	i32 num;
	fc_string_hash_t* names;
	fg_spawn_info_prop_value_t* values;
} fg_spawn_info_properties_t;

// info for initialising game object
typedef struct fg_spawn_info_t
{
	// unique ID of the game object, can be used in scripts
	fc_string_hash_t gameObjectName;
	
	// key-value list of properties (only different than default)
	fg_spawn_info_properties_t props;
	
} fg_spawn_info_t;

// get key-value properties through these functions
f32 fg_spawn_info_get_float(const fg_spawn_info_t* info, fc_string_hash_t name, f32 defaultValue);
i32 fg_spawn_info_get_int(const fg_spawn_info_t* info, fc_string_hash_t name, i32 defaultValue);
fc_string_hash_t fg_spawn_info_get_string_hash(const fg_spawn_info_t* info, fc_string_hash_t name, fc_string_hash_t defaultValue);

// stack allocator for game object used on init
typedef struct fg_stack_allocator_t
{
	void* ptr;
	i32 size;
	i32 capacity;
} fg_stack_allocator_t;

void* fg_stack_alloc(fg_stack_allocator_t* allocator, i32 size);

typedef enum fg_update_bucket_t
{
	FG_UPDATE_BUCKET_CHARACTERS = 0,
	FG_UPDATE_BUCKET_COUNT
} fg_update_bucket_t;

typedef struct fg_resource_register_t fg_resource_register_t;
typedef struct fg_systems_register_t fg_systems_register_t;
typedef struct fg_game_object_2_t fg_game_object_2_t;

typedef struct fg_game_object_init_ctx_t
{
	// all the information from spawner
	const fg_spawn_info_t* info;
	
	// each object can allocate its memory on stack in level heap during init
	fc_alloc_callbacks_t* stackAlloc;
	
	// register of all available resources
	const fg_resource_register_t* resources;
	
	// all systems
	const fg_systems_register_t* systems;
	
	// current global time
	f64 globalTime;	// todo: remove?
} fg_game_object_init_ctx_t;

typedef bool (*fg_game_object_init_func_t)(fg_game_object_2_t* gameObject, fg_game_object_init_ctx_t* ctx);

typedef struct fg_game_object_update_ctx_t
{
	f32 dt;
} fg_game_object_update_ctx_t;

typedef void (*fg_game_object_update_func_t)(fg_game_object_2_t* gameObject, fg_game_object_update_ctx_t* ctx);

typedef struct fg_game_object_funcs_t
{
	fg_game_object_init_func_t init;
	fg_game_object_update_func_t update;
} fg_game_object_funcs_t;

// handle for the game object data
typedef struct fg_game_object_2_t
{
	const fg_game_object_funcs_t* fn;
	fc_string_hash_t name;
} fg_game_object_2_t;

typedef struct fc_binary_buffer_t fc_binary_buffer_t;
typedef struct fa_anim_clip_t fa_anim_clip_t;
typedef struct fa_rig_t fa_rig_t;
typedef struct fr_proxy_t fr_proxy_t;

FUR_DEFINE_MAP_TYPE(fg_resource_map_scripts_t, fc_string_hash_t, const fc_binary_buffer_t*);
FUR_DEFINE_MAP_TYPE(fg_resource_map_anim_clip_t, fc_string_hash_t, const fa_anim_clip_t*);
FUR_DEFINE_MAP_TYPE(fg_resource_map_rig_t, fc_string_hash_t, const fa_rig_t*);
FUR_DEFINE_MAP_TYPE(fg_resource_map_mesh_t, fc_string_hash_t, const fr_proxy_t*);

typedef struct fg_resource_register_t
{
	fg_resource_map_scripts_t scripts;
	fg_resource_map_anim_clip_t animations;
	fg_resource_map_rig_t rigs;
	fg_resource_map_mesh_t meshes;
} fg_resource_register_t;

const fa_anim_clip_t* fg_resource_find_anim(const fg_resource_register_t* reg, fc_string_hash_t name);
const fc_binary_buffer_t* fg_resource_find_script(const fg_resource_register_t* reg, fc_string_hash_t name);
const fa_rig_t* fg_resource_find_rig(const fg_resource_register_t* reg, fc_string_hash_t name);
const fr_proxy_t* fg_resource_find_mesh(const fg_resource_register_t* reg, fc_string_hash_t name);

void fg_resource_add_anim(fg_resource_register_t* reg, fc_string_hash_t name, const fa_anim_clip_t* res);
void fg_resource_add_script(fg_resource_register_t* reg, fc_string_hash_t name, const fc_binary_buffer_t* res);
void fg_resource_add_rig(fg_resource_register_t* reg, fc_string_hash_t name, const fa_rig_t* res);
void fg_resource_add_mesh(fg_resource_register_t* reg, fc_string_hash_t name, const fr_proxy_t* res);

typedef struct fr_renderer_t fr_renderer_t;
typedef struct fa_anim_sys_t fa_anim_sys_t;

typedef struct fg_systems_register_t
{
	fr_renderer_t* renderer;
	fa_anim_sys_t* animation;
} fg_systems_register_t;

// used for defining how to initialise specific type of game object
typedef struct fg_type_factory_t
{
	// game object custom functions
	fg_game_object_funcs_t fn;
	
	// size information for relocatable memory management, see level heap
	u32 memoryMaxSize;
	
	// which bucket is the game object updated in
	fg_update_bucket_t updateBucket;
} fg_type_factory_t;

void fg_type_factory_register_new(fc_string_hash_t typeName, fg_type_factory_t factory);

// use index to get pointer to the game object, then compare go.name to name to f64 check
typedef struct fg_game_object_handle_t
{
	// index of the game object slot
	i32 index;
	
	// unique name
	fc_string_hash_t name;
} fg_game_object_handle_t;

FUR_DEFINE_ARRAY_TYPE(fg_game_object_handle_array_t, fg_game_object_handle_t);
FUR_DEFINE_ARRAY_TYPE(fc_game_object_array_t, fg_game_object_2_t);

typedef struct fg_spawner_t
{
	fc_string_hash_t typeName;
	fc_string_hash_t name;
	fg_spawn_info_t info;
} fg_spawner_t;

#define MAX_GAME_OBJECTS_SPAWNED 2048

typedef struct fg_game_object_info_storage_t
{
	fg_game_object_2_t* ptr[MAX_GAME_OBJECTS_SPAWNED];
	const fg_spawner_t* spawner[MAX_GAME_OBJECTS_SPAWNED];
	
	// number of initialised game objects
	i32 numInit;
	
	// total number of game objects
	i32 num;
} fg_game_object_info_storage_t;

typedef struct fg_world_t
{
	// list of all game object info (use game object index to get data)
	fg_game_object_info_storage_t gameObjects;
	
	// list of all game objects
	fc_game_object_array_t allGameObjects;

	// memory for dynamic objects (game objects), does not include resources like animation, meshes, textures
	fc_mem_rel_heap_alloc_t* levelHeap;
	
	// all available resources in the world
	fg_resource_register_t resources;
	
	// low-level systems
	fg_systems_register_t systems;
	
	// game objects in update buckets
	fg_game_object_handle_array_t buckets[FG_UPDATE_BUCKET_COUNT];
	
	// true if something is pending to spawn
	bool hasSpawnScheduled;
	
} fg_world_t;

void fg_world_init(fg_world_t* world, fc_alloc_callbacks_t* pAllocCallbacks);
void fg_world_release(fg_world_t* world, fc_alloc_callbacks_t* pAllocCallbacks);

typedef struct fg_world_update_ctx_t
{
	f32 dt;
} fg_world_update_ctx_t;

void fg_world_update(fg_world_t* world, fg_world_update_ctx_t* ctx, fg_update_bucket_t bucket);

// all resources should be already loaded at the time of spawn call
fg_game_object_handle_t fg_spawn(const fg_spawner_t* spawner, fg_world_t* world);
void fg_despawn(fg_game_object_handle_t handle, fg_world_t* world);

#ifdef __cplusplus
}
#endif // __cplusplus
