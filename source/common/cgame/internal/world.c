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
	int stuff;
} fg_world_global_t;

fg_world_global_t g_worldExtended;

typedef struct fg_type_factory_register_t
{
	fc_string_hash_t typeNames[MAX_TYPE_FACTORIES];
	fg_type_factory_t factories[MAX_TYPE_FACTORIES];
	int32_t numFactories;
} fg_type_factory_register_t;

fg_type_factory_register_t g_typeFactories;

void fg_type_factory_register_new(fc_string_hash_t typeName, fg_type_factory_t factory)
{
	FUR_ASSERT(g_typeFactories.numFactories < MAX_TYPE_FACTORIES);
	
	const int32_t idx = g_typeFactories.numFactories;
	g_typeFactories.factories[idx] = factory;
	g_typeFactories.typeNames[idx] = typeName;
	g_typeFactories.numFactories++;
}

const fg_type_factory_t* fg_type_factory_find(fc_string_hash_t typeName)
{
	for(int32_t i=0; i<g_typeFactories.numFactories; ++i)
	{
		if(g_typeFactories.typeNames[i] == typeName)
		{
			return &g_typeFactories.factories[i];
		}
	}
	
	FUR_ASSERT(false);	// can't find matching type factory
	
	return NULL;
}

int32_t fg_spawn_info_find_prop_index(const fg_spawn_info_properties_t* props, fc_string_hash_t name)
{
	for(int32_t i=0; i<props->num; ++i)
	{
		if(props->names[i] == name)
			return i;
	}
	
	return -1;
}

float fg_spawn_info_get_float(const fg_spawn_info_t* info, fc_string_hash_t name, float defaultValue)
{
	const int32_t idx = fg_spawn_info_find_prop_index(&info->props, name);
	
	if(idx != -1)
		return info->props.values[idx].asFloat;
	
	return defaultValue;
}

int32_t fg_spawn_info_get_int(const fg_spawn_info_t* info, fc_string_hash_t name, int32_t defaultValue)
{
	const int32_t idx = fg_spawn_info_find_prop_index(&info->props, name);
	
	if(idx != -1)
		return info->props.values[idx].asInt32;
	
	return defaultValue;
}

fc_string_hash_t fg_spawn_info_get_string_hash(const fg_spawn_info_t* info, fc_string_hash_t name, fc_string_hash_t defaultValue)
{
	const int32_t idx = fg_spawn_info_find_prop_index(&info->props, name);
	
	if(idx != -1)
		return info->props.values[idx].asStringHash;
	
	return defaultValue;
}

void fg_game_object_handle_array_alloc(fg_game_object_handle_array_t* arr, int32_t capacity, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_ASSERT(arr->data == NULL);
	
	arr->data = FUR_ALLOC_ARRAY_AND_ZERO(fg_game_object_handle_t, capacity, 0, FC_MEMORY_SCOPE_GAME, pAllocCallbacks);
	arr->num = 0;
	arr->capacity = capacity;
}

void fg_game_object_handle_array_free(fg_game_object_handle_array_t* arr, fc_alloc_callbacks_t* pAllocCallbacks)
{
	FUR_ASSERT(arr->data != NULL);
	FUR_FREE(arr->data, pAllocCallbacks);
	arr->num = 0;
	arr->capacity = 0;
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
	fg_game_object_handle_array_alloc(&world->buckets[FG_UPDATE_BUCKET_CHARACTERS], 256, pAllocCallbacks);
}

void fg_world_release(fg_world_t* world, fc_alloc_callbacks_t* pAllocCallbacks)
{
	// release level heap
	FUR_FREE(world->levelHeap->buffer, pAllocCallbacks);
	FUR_FREE(world->levelHeap, pAllocCallbacks);
	
	// release update buckets
	fg_game_object_handle_array_free(&world->buckets[FG_UPDATE_BUCKET_CHARACTERS], pAllocCallbacks);
	
	// release resources
	for(int32_t i=0; i<world->resources.numAnimations; ++i)
	{
		fa_anim_clip_release((fa_anim_clip_t*)world->resources.animations[i], pAllocCallbacks);
	}
	
	for(int32_t i=0; i<world->resources.numRigs; ++i)
	{
		fa_rig_release((fa_rig_t*)world->resources.rigs[i], pAllocCallbacks);
	}
	
	for(int32_t i=0; i<world->resources.numScripts; ++i)
	{
		fc_release_binary_buffer((fc_binary_buffer_t*)world->resources.scripts[i], pAllocCallbacks);
	}
}

void fg_world_update(fg_world_t* world, fg_world_update_ctx_t* ctx, fg_update_bucket_t bucket)
{
	fg_game_object_info_storage_t* info = &world->gameObjects;
	
	// scheduled spawn game objects
	{
		for(int32_t i=info->numInit; i<info->num; ++i)
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
	for(int32_t i=0; i<FG_UPDATE_BUCKET_COUNT; ++i)
	{
		fg_game_object_handle_array_t* bucket = &world->buckets[i];
		
		// go through all game objects within a bucket
		for(int32_t idxHandle=0; idxHandle<bucket->num; ++i)
		{
			fg_game_object_handle_t handle = bucket->data[idxHandle];
			
			// get game object info
			fg_game_object_2_t* gameObject = info->ptr[handle.index];
			
			// update game object
			gameObject->fn->update(gameObject, &updateCtx);
		}
	}
}

void* fg_stack_alloc(fg_stack_allocator_t* allocator, int32_t size)
{
	FUR_ASSERT(allocator->size + size <= allocator->capacity);
	void* ptr = allocator->ptr;
	allocator->ptr = (uint8_t*)allocator->ptr + size;
	allocator->size += size;
	
	memset(ptr, 0, size);
	
	return ptr;
}

const fa_anim_clip_t* fg_resource_find_anim(const fg_resource_register_t* reg, fc_string_hash_t name)
{
	for(int32_t i=0; i<reg->numAnimations; ++i)
	{
		if(reg->animationsNames[i] == name)
			return reg->animations[i];
	}
	
	FUR_ASSERT(false); // can't find resource
	
	return NULL;
}

const fc_binary_buffer_t* fg_resource_find_script(const fg_resource_register_t* reg, fc_string_hash_t name)
{
	for(int32_t i=0; i<reg->numScripts; ++i)
	{
		if(reg->scriptsNames[i] == name)
			return reg->scripts[i];
	}
	
	FUR_ASSERT(false); // can't find resource
	
	return NULL;
}

const fa_rig_t* fg_resource_find_rig(const fg_resource_register_t* reg, fc_string_hash_t name)
{
	for(int32_t i=0; i<reg->numRigs; ++i)
	{
		if(reg->rigsNames[i] == name)
			return reg->rigs[i];
	}
	
	FUR_ASSERT(false); // can't find resource
	
	return NULL;
}

const fr_proxy_t* fg_resource_find_mesh(const fg_resource_register_t* reg, fc_string_hash_t name)
{
	for(int32_t i=0; i<reg->numMeshes; ++i)
	{
		if(reg->meshesNames[i] == name)
			return reg->meshes[i];
	}
	
	FUR_ASSERT(false); // can't find resource
	
	return NULL;
}

void fg_resource_add_anim(fg_resource_register_t* reg, fc_string_hash_t name, const fa_anim_clip_t* res)
{
	FUR_ASSERT(reg->numAnimations < FG_MAX_NUM_ANIMATIONS);
	
	reg->animations[reg->numAnimations] = res;
	reg->animationsNames[reg->numAnimations] = name;
	reg->numAnimations++;
}

void fg_resource_add_script(fg_resource_register_t* reg, fc_string_hash_t name, const fc_binary_buffer_t* res)
{
	FUR_ASSERT(reg->numScripts < FG_MAX_NUM_SCRIPTS);
	
	reg->scripts[reg->numScripts] = res;
	reg->scriptsNames[reg->numScripts] = name;
	reg->numScripts++;
}

void fg_resource_add_rig(fg_resource_register_t* reg, fc_string_hash_t name, const fa_rig_t* res)
{
	FUR_ASSERT(reg->numRigs < FG_MAX_NUM_RIGS);
	
	reg->rigs[reg->numRigs] = res;
	reg->rigsNames[reg->numRigs] = name;
	reg->numRigs++;
}

void fg_resource_add_mesh(fg_resource_register_t* reg, fc_string_hash_t name, const fr_proxy_t* res)
{
	FUR_ASSERT(reg->numMeshes < FG_MAX_NUM_MESHES);
	
	reg->meshes[reg->numMeshes] = res;
	reg->meshesNames[reg->numMeshes] = name;
	reg->numMeshes++;
}

fg_game_object_handle_t fg_game_object_storage_find_free_handle(fg_game_object_info_storage_t* storage)
{
	fg_game_object_handle_t handle = {0};
	handle.index = -1;
	
	// find free storage slot
	for(int32_t i=0; i<MAX_GAME_OBJECTS_SPAWNED; ++i)
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


