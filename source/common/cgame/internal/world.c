/* Copyright (c) Furball Cat */

#include "world.h"
#include "ccore/public.h"
#include "ccore/buffer.h"
#include "canim/public.h"
#include <string.h>

#define LEVEL_HEAP_MEMORY_CAPACITY 16 * 1024 * 1024
#define MAX_TYPE_FACTORIES 128

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
	
	// allocate level heap
	world->levelHeap.memory = FUR_ALLOC(LEVEL_HEAP_MEMORY_CAPACITY, 8, FC_MEMORY_SCOPE_GAME, pAllocCallbacks);
	world->levelHeap.freePtr = world->levelHeap.memory;
	world->levelHeap.memoryCapacity = LEVEL_HEAP_MEMORY_CAPACITY;
	world->levelHeap.allocatedObjects = FUR_ALLOC_ARRAY_AND_ZERO(void*, MAX_GAME_OBJECTS_SPAWNED, 0, FC_MEMORY_SCOPE_GAME, pAllocCallbacks);
	world->levelHeap.numAllocatedObjects = 0;
	
	// allocate update buckets
	fg_game_object_handle_array_alloc(&world->buckets[FG_UPDATE_BUCKET_CHARACTERS], 256, pAllocCallbacks);
}

void fg_world_release(fg_world_t* world, fc_alloc_callbacks_t* pAllocCallbacks)
{
	// release level heap
	FUR_FREE(world->levelHeap.memory, pAllocCallbacks);
	FUR_FREE(world->levelHeap.allocatedObjects, pAllocCallbacks);
	
	// release update buckets
	fg_game_object_handle_array_free(&world->buckets[FG_UPDATE_BUCKET_CHARACTERS], pAllocCallbacks);
	
	// release resources
	for(int32_t i=0; i<world->resources.numAnimations; ++i)
	{
		fa_anim_clip_release((fa_anim_clip_t*)world->resources.animations[i], pAllocCallbacks);
	}
	
	for(int32_t i=0; i<world->resources.numScripts; ++i)
	{
		fc_release_binary_buffer((fc_binary_buffer_t*)world->resources.scripts[i], pAllocCallbacks);
	}
}

void* fg_level_heap_alloc_object(fg_level_heap_t* levelHeap, int32_t maxSize)
{
	FUR_ASSERT(levelHeap->numAllocatedObjects < MAX_GAME_OBJECTS_SPAWNED);
	FUR_ASSERT(levelHeap->freePtr + maxSize < levelHeap->memory + levelHeap->memoryCapacity);
	
	void* objectPtr = levelHeap->freePtr;
	
	const int32_t idx = levelHeap->numAllocatedObjects;
	levelHeap->allocatedObjects[idx] = objectPtr;
	
	levelHeap->freePtr += maxSize;
	levelHeap->numAllocatedObjects++;
	
	return objectPtr;
}

void fg_world_update(fg_world_t* world, fg_world_update_ctx_t* ctx, fg_update_bucket_t bucket)
{
	fg_game_object_info_storage_t* info = &world->gameObjects;
	
	// scheduled spawn game objects
	if(world->hasSpawnScheduled)
	{
		for(int32_t i=0; i<MAX_GAME_OBJECTS_SPAWNED; ++i)
		{
			fg_game_object_2_t* gameObject = info->ptr[i];
			if(gameObject != NULL)
			{
				const fg_type_factory_t* factory = fg_type_factory_find(info->spawner[i]->typeName);
				
				// game object can allocate its memory on level heap, through this stack allocator
				fg_stack_allocator_t stackAlloc = {};
				stackAlloc.size = sizeof(fg_game_object_2_t);
				stackAlloc.ptr = world->levelHeap.freePtr;
				stackAlloc.size = factory->memoryMaxSize;
				
				fg_game_object_init_ctx_t initCtx = {};
				initCtx.info = &info->spawner[i]->info;
				initCtx.resources = &world->resources;
				initCtx.stackAlloc = &stackAlloc;
				
				gameObject->fn->init(gameObject, &initCtx);
				
				// reserve memory that is actually used by the game object
				fg_level_heap_alloc_object(&world->levelHeap, stackAlloc.size);
			}
		}
	}
	
	fg_game_object_update_ctx_t updateCtx = {};
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
	allocator->ptr += size;
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
	
	return NULL;
}

const fc_binary_buffer_t* fg_resource_find_script(const fg_resource_register_t* reg, fc_string_hash_t name)
{
	for(int32_t i=0; i<reg->numScripts; ++i)
	{
		if(reg->scriptsNames[i] == name)
			return reg->scripts[i];
	}
	
	return NULL;
}

fg_game_object_handle_t fg_game_object_storage_find_free_handle(fg_game_object_info_storage_t* storage)
{
	fg_game_object_handle_t handle = {};
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
	
	fg_game_object_2_t* gameObject = (fg_game_object_2_t*)fg_level_heap_alloc_object(&world->levelHeap, factory->memoryMaxSize);
	gameObject->fn = &factory->fn;
	gameObject->name = spawner->name;
	
	fg_game_object_handle_t handle = fg_game_object_storage_find_free_handle(&world->gameObjects);
	FUR_ASSERT(handle.index != -1);
	
	handle.name = spawner->name;
	
	fg_game_object_info_storage_t* info = &world->gameObjects;
	info->ptr[handle.index] = gameObject;
	info->spawner[handle.index] = spawner;
	
	world->hasSpawnScheduled = true;
	
	return handle;
}

void fg_despawn(fg_game_object_handle_t handle, fg_world_t* world)
{
	// todo: implement
}

void fg_relocate_pointer(void** ptr, int32_t delta, void* lowerBound, void* upperBound)
{
	if(lowerBound <= *ptr && *ptr < upperBound)
	{
		*ptr = *ptr + delta;
	}
}
