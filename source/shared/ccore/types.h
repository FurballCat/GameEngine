/* Copyright (c) 2016-2023 Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef float f32;
typedef double f64;

typedef u32 FcStringId;

typedef union FcVariant
{
	FcStringId asStringHash;
	i32 asInt32;
	bool asBool;
	f32 asFloat;
} FcVariant;

typedef enum FcResult
{
	FC_SUCCESS = 0,
	FC_ERROR_UNKNOWN,
	FC_ERROR_SHADER_CREATION,
	FC_ERROR_GPU,
	FC_ERROR_PHYSICS_CREATION,
	FC_ERROR_ENGINE_CREATION,
} FcResult;

#define FUR_DEFINE_ARRAY_TYPE(_arrayType, _elemType) \
	typedef struct _arrayType \
	{ \
		_elemType* data; \
		u32 capacity; \
		u32 num; \
		u32 stride; \
	} _arrayType

#define FUR_DEFINE_MAP_TYPE(_mapType, _keyType, _elemType) \
	typedef struct _mapType \
	{ \
		_keyType* keys; \
		_elemType* elems; \
		u32 capacity; \
		u32 num; \
		u16 keyStride; \
		u16 elemStride; \
	} _mapType

#ifdef __cplusplus
}
#endif // __cplusplus
