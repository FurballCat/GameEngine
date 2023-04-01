/* Copyright (c) Furball Cat */

#include "world.h"
#include "ccore/public.h"
#include "ccore/buffer.h"
#include "canim/public.h"
#include <string.h>

#define LEVEL_HEAP_MEMORY_CAPACITY 16 * 1024 * 1024
#define MAX_TYPE_FACTORIES 128

typedef struct fg_world_global_t
{
	i32 stuff;
} fg_world_global_t;

fg_world_global_t g_worldExtended;

typedef struct fg_type_factory_register_t
{
	fc_string_hash_t typeNames[MAX_TYPE_FACTORIES];
	fg_type_factory_t factories[MAX_TYPE_FACTORIES];
	i32 numFactories;
} fg_type_factory_register_t;

fg_type_factory_register_t g_typeFactories;

void fg_type_factory_register_new(fc_string_hash_t typeName, fg_type_factory_t factory)
{
	FUR_ASSERT(g_typeFactories.numFactories < MAX_TYPE_FACTORIES);
	
	const i32 idx = g_typeFactories.numFactories;
	g_typeFactories.factories[idx] = factory;
	g_typeFactories.typeNames[idx] = typeName;
	g_typeFactories.numFactories++;
}

const fg_type_factory_t* fg_type_factory_find(fc_string_hash_t typeName)
{
	for(i32 i=0; i<g_typeFactories.numFactories; ++i)
	{
		if(g_typeFactories.typeNames[i] == typeName)
		{
			return &g_typeFactories.factories[i];
		}
	}
	
	FUR_ASSERT(false);	// can't find matching type factory
	
	return NULL;
}

i32 fg_spawn_info_find_prop_index(const fg_spawn_info_properties_t* props, fc_string_hash_t name)
{
	for(i32 i=0; i<props->num; ++i)
	{
		if(props->names[i] == name)
			return i;
	}
	
	return -1;
}

f32 fg_spawn_info_get_float(const fg_spawn_info_t* info, fc_string_hash_t name, f32 defaultValue)
{
	const i32 idx = fg_spawn_info_find_prop_index(&info->props, name);
	
	if(idx != -1)
		return info->props.values[idx].asFloat;
	
	return defaultValue;
}

i32 fg_spawn_info_get_int(const fg_spawn_info_t* info, fc_string_hash_t name, i32 defaultValue)
{
	const i32 idx = fg_spawn_info_find_prop_index(&info->props, name);
	
	if(idx != -1)
		return info->props.values[idx].asInt32;
	
	return defaultValue;
}

fc_string_hash_t fg_spawn_info_get_string_hash(const fg_spawn_info_t* info, fc_string_hash_t name, fc_string_hash_t defaultValue)
{
	const i32 idx = fg_spawn_info_find_prop_index(&info->props, name);
	
	if(idx != -1)
		return info->props.values[idx].asStringHash;
	
	return defaultValue;
}

void fg_world_init(fg_world_t* world, fc_alloc_callbacks_t* pAllocCallbacks)
{
	// init by zero
	memset(world, 0, sizeof(fg_world_t));
	
	world->levelHeap = FUR_ALLOC_AND_ZERO(sizeof(fc_mem_rel_heap_alloc_t), 0, FC_MEMORY_SCOPE_GAME, pAllocCallbacks);
	
	// allocate level heap
	world->levelHeap->buffer = FUR_ALLOC(LEVEL_HEAP_MEMORY_CAPACITY, 8, FC_MEMORY_SCOPE_GAME, pAllocCallbacks);
	world->levelHeap->freePtr = world->levelHeap->buffer;
	world->levelHeap->capacity = LEVEL_HEAP_MEMORY_CAPACITY;
	world->levelHeap->size = 0;
	
	// allocate update buckets
	fc_array_alloc_and_zero(&world->buckets[FG_UPDATE_BUCKET_CHARACTERS], fg_game_object_handle_t, 256, 8, FC_MEMORY_SCOPE_GAME, pAllocCallbacks);

	// allocate resource maps
	fc_map_alloc_and_zero(&world->resources.scripts, fc_string_hash_t, const fc_binary_buffer_t, 512, 0, FC_MEMORY_SCOPE_GAME, pAllocCallbacks);
	fc_map_alloc_and_zero(&world->resources.animations, fc_string_hash_t, const fa_anim_clip_t*, 1024, 0, FC_MEMORY_SCOPE_GAME, pAllocCallbacks);
	fc_map_alloc_and_zero(&world->resources.rigs, fc_string_hash_t, const fa_rig_t*, 128, 0, FC_MEMORY_SCOPE_GAME, pAllocCallbacks);
	fc_map_alloc_and_zero(&world->resources.meshes, fc_string_hash_t, const fr_proxy_t*, 2048, 0, FC_MEMORY_SCOPE_GAME, pAllocCallbacks);
}

void fg_world_release(fg_world_t* world, fc_alloc_callbacks_t* pAllocCallbacks)
{
	// release level heap
	FUR_FREE(world->levelHeap->buffer, pAllocCallbacks);
	FUR_FREE(world->levelHeap, pAllocCallbacks);
	
	// release update buckets
	fc_array_free(&world->buckets[FG_UPDATE_BUCKET_CHARACTERS], pAllocCallbacks);
	
	// release resources
	for(u32 i=0; i<world->resources.animations.num; ++i)
	{
		fa_anim_clip_release((fa_anim_clip_t*)world->resources.animations.elems[i], pAllocCallbacks);
	}
	
	for(u32 i=0; i<world->resources.rigs.num; ++i)
	{
		fa_rig_release((fa_rig_t*)world->resources.rigs.elems[i], pAllocCallbacks);
	}
	
	for(u32 i=0; i<world->resources.scripts.num; ++i)
	{
		fc_release_binary_buffer((fc_binary_buffer_t*)world->resources.scripts.elems[i], pAllocCallbacks);
	}

	// release resource maps
	fc_map_free(&world->resources.scripts, pAllocCallbacks);
	fc_map_free(&world->resources.animations, pAllocCallbacks);
	fc_map_free(&world->resources.rigs, pAllocCallbacks);
	fc_map_free(&world->resources.meshes, pAllocCallbacks);
}

void fg_world_update(fg_world_t* world, fg_world_update_ctx_t* ctx, fg_update_bucket_t bucket)
{
	fg_game_object_info_storage_t* info = &world->gameObjects;
	
	// scheduled spawn game objects
	{
		for(i32 i=info->numInit; i<info->num; ++i)
		{
			fg_game_object_2_t* gameObject = info->ptr[i];
			if(gameObject != NULL)
			{
				//const fg_type_factory_t* factory = fg_type_factory_find(info->spawner[i]->typeName);
				
				// game object can allocate its memory on level heap, through this stack allocator
				fc_alloc_callbacks_t levelHeapAlloc = fc_mem_rel_heap_get_callbacks(world->levelHeap);
				
				fg_game_object_init_ctx_t initCtx = {0};
				initCtx.info = &info->spawner[i]->info;
				initCtx.resources = &world->resources;
				initCtx.systems = &world->systems;
				initCtx.stackAlloc = &levelHeapAlloc;
				
				gameObject->fn->init(gameObject, &initCtx);
				
				// reserve memory that is actually used by the game object
				// ...
			}
		}
		
		info->numInit = info->num;
	}
	
	fg_game_object_update_ctx_t updateCtx = {0};
	updateCtx.dt = ctx->dt;
	
	// go through all update buckets
	for(i32 i=0; i<FG_UPDATE_BUCKET_COUNT; ++i)
	{
		fg_game_object_handle_array_t* bucket = &world->buckets[i];
		
		// go through all game objects within a bucket
		for(i32 idxHandle=0; idxHandle<bucket->num; ++i)
		{
			fg_game_object_handle_t handle = bucket->data[idxHandle];
			
			// get game object info
			fg_game_object_2_t* gameObject = info->ptr[handle.index];
			
			// update game object
			gameObject->fn->update(gameObject, &updateCtx);
		}
	}
}

void* fg_stack_alloc(fg_stack_allocator_t* allocator, i32 size)
{
	FUR_ASSERT(allocator->size + size <= allocator->capacity);
	void* ptr = allocator->ptr;
	allocator->ptr = (u8*)allocator->ptr + size;
	allocator->size += size;
	
	memset(ptr, 0, size);
	
	return ptr;
}

const fa_anim_clip_t* fg_resource_find_anim(const fg_resource_register_t* reg, fc_string_hash_t name)
{
	const fa_anim_clip_t* result = (const fa_anim_clip_t*)fc_map_find(&reg->animations, &name);
	FUR_ASSERT(result);
	return result;
}

const fc_binary_buffer_t* fg_resource_find_script(const fg_resource_register_t* reg, fc_string_hash_t name)
{
	const fc_binary_buffer_t* result = (const fc_binary_buffer_t*)fc_map_find(&reg->scripts, &name);
	FUR_ASSERT(result);
	return result;
}

const fa_rig_t* fg_resource_find_rig(const fg_resource_register_t* reg, fc_string_hash_t name)
{
	const fa_rig_t* result = (const fa_rig_t*)fc_map_find(&reg->rigs, &name);
	FUR_ASSERT(result);
	return result;
}

const fr_proxy_t* fg_resource_find_mesh(const fg_resource_register_t* reg, fc_string_hash_t name)
{
	const fr_proxy_t* result = (const fr_proxy_t*)fc_map_find(&reg->meshes, &name);
	FUR_ASSERT(result);
	return result;
}

void fg_resource_add_anim(fg_resource_register_t* reg, fc_string_hash_t name, const fa_anim_clip_t* res)
{
	fc_map_insert(&reg->animations, &name, &res);
}

void fg_resource_add_script(fg_resource_register_t* reg, fc_string_hash_t name, const fc_binary_buffer_t* res)
{
	fc_map_insert(&reg->scripts, &name, &res);
}

void fg_resource_add_rig(fg_resource_register_t* reg, fc_string_hash_t name, const fa_rig_t* res)
{
	fc_map_insert(&reg->rigs, &name, &res);
}

void fg_resource_add_mesh(fg_resource_register_t* reg, fc_string_hash_t name, const fr_proxy_t* res)
{
	fc_map_insert(&reg->meshes, &name, &res);
}

fg_game_object_handle_t fg_game_object_storage_find_free_handle(fg_game_object_info_storage_t* storage)
{
	fg_game_object_handle_t handle = {0};
	handle.index = -1;
	
	// find free storage slot
	for(i32 i=0; i<MAX_GAME_OBJECTS_SPAWNED; ++i)
	{
		if(storage->ptr[i] == NULL)
		{
			handle.index = i;
			break;
		}
	}
	
	return handle;
}

fg_game_object_handle_t fg_spawn(const fg_spawner_t* spawner, fg_world_t* world)
{
	const fg_type_factory_t* factory = fg_type_factory_find(spawner->typeName);
	
	fc_alloc_callbacks_t alloc = fc_mem_rel_heap_get_callbacks(world->levelHeap);
	fg_game_object_2_t* gameObject = FUR_ALLOC_AND_ZERO(factory->memoryMaxSize, 0, FC_MEMORY_SCOPE_GAME, &alloc);
	gameObject->fn = &factory->fn;
	gameObject->name = spawner->name;
	
	fg_game_object_handle_t handle = fg_game_object_storage_find_free_handle(&world->gameObjects);
	FUR_ASSERT(handle.index != -1);
	
	handle.name = spawner->name;
	
	fg_game_object_info_storage_t* info = &world->gameObjects;
	info->ptr[handle.index] = gameObject;
	info->spawner[handle.index] = spawner;
	
	world->gameObjects.num++;
	
	return handle;
}

void fg_despawn(fg_game_object_handle_t handle, fg_world_t* world)
{
	// todo: implement
}


