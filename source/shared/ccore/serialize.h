/* Copyright (c) Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus
	
#include "types.h"
#include "api.h"

// add custom types here
#if PLATFORM_OSX

#define fc_serialize(_serializer, _property) _Generic((_property), \
	int8_t*: fc_serialize_int8,	\
	int16_t*: fc_serialize_int16, \
	i32*: fc_serialize_int32, \
	int64_t*: fc_serialize_int64, \
	u8*: fc_serialize_uint8,	\
	u16*: fc_serialize_uint16, \
	u32*: fc_serialize_uint32, \
	uint64_t*: fc_serialize_uint64, \
	f32*: fc_serialize_float, \
	f64*: fc_serialize_double, \
	fa_anim_curve_t*: fa_serialize_anim_curve \
)(_serializer, _property)

#elif PLATFORM_WINDOWS

#define fc_serialize(_serializer, _property) _Generic((_property), \
	int8_t*: fc_serialize_i8,	\
	int16_t*: fc_serialize_i16, \
	i32*: fc_serialize_i32, \
	int64_t*: fc_serialize_i64, \
	u8*: fc_serialize_u8,	\
	u16*: fc_serialize_u16, \
	u32*: fc_serialize_u32, \
	uint64_t*: fc_serialize_u64, \
	f32*: fc_serialize_f32, \
	f64*: fc_serialize_f64, \
	fa_anim_curve_t*: fa_serialize_anim_curve, \
	enum fm_axis_t*: fc_serialize_u32 \
)(_serializer, _property)

#endif

#define FUR_SER_VERSION(_versionLatest)	\
	if(pSerializer->isWriting) \
	{ \
		pSerializer->version = _versionLatest;	\
		fc_serialize(pSerializer, &pSerializer->version); \
	} \
	else \
	{ \
		fc_serialize(pSerializer, &pSerializer->version); \
	}

#define FUR_SER_ADD(_versionAdded, _property) \
	if(pSerializer->version >= _versionAdded) \
	{\
		fc_serialize(pSerializer, &_property);\
	}

#define FUR_SER_ADD_BUFFER(_versionAdded, _ptr, _sizeInBytes) \
	if(pSerializer->version >= _versionAdded) \
	{\
		fc_serialize_buffer(pSerializer, _ptr, _sizeInBytes);\
	}

#define FUR_SER_REM(_versionAdded, _versionRemoved, _type, _property, _defaultValue) \
	_type _property = _defaultValue; \
	if(pSerializer->version >= _versionAdded && pSerializer->version < _versionRemoved)	\
	{	\
		fc_serialize(pSerializer, &_property); \
	}

#ifdef PLATFORM_OSX
typedef struct __sFILE FILE;
#elif PLATFORM_WINDOWS
typedef struct _iobuf FILE;
#else
#endif

typedef struct fc_depot_t fc_depot_t;
typedef u64 fc_file_path_t;
typedef struct fc_file_t fc_file_t;

// interface - use this + FUR_SER_ADD, FUR_SER_REM macros
typedef struct fc_serializer_t
{
	i32 version;
	fc_file_t* file;
	bool isWriting;
} fc_serializer_t;

// simple type serialization functions
void fc_serialize_i8(fc_serializer_t* pSerializer, i8* prop);
void fc_serialize_i16(fc_serializer_t* pSerializer, i16* prop);
void fc_serialize_i32(fc_serializer_t* pSerializer, i32* prop);
void fc_serialize_i64(fc_serializer_t* pSerializer, i64* prop);
void fc_serialize_u8(fc_serializer_t* pSerializer, u8* prop);
void fc_serialize_u16(fc_serializer_t* pSerializer, u16* prop);
void fc_serialize_u32(fc_serializer_t* pSerializer, u32* prop);
void fc_serialize_u64(fc_serializer_t* pSerializer, u64* prop);
void fc_serialize_f32(fc_serializer_t* pSerializer, f32* prop);
void fc_serialize_f64(fc_serializer_t* pSerializer, f64* prop);

void fc_serialize_buffer(fc_serializer_t* pSerializer, void* ptr, u32 size);

typedef struct fa_anim_curve_t fa_anim_curve_t;
void fa_serialize_anim_curve(fc_serializer_t* pSerializer, fa_anim_curve_t* animCurve);

#ifdef __cplusplus
}
#endif // __cplusplus
