/* Copyright (c) Furball Cat */

#pragma once

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus
	
#include <inttypes.h>
#include <stdbool.h>
#include "api.h"

// add custom types here
#if PLATFORM_OSX

#define fc_serialize(_serializer, _property) _Generic((_property), \
	int8_t*: fc_serialize_int8,	\
	int16_t*: fc_serialize_int16, \
	int32_t*: fc_serialize_int32, \
	int64_t*: fc_serialize_int64, \
	uint8_t*: fc_serialize_uint8,	\
	uint16_t*: fc_serialize_uint16, \
	uint32_t*: fc_serialize_uint32, \
	uint64_t*: fc_serialize_uint64, \
	float*: fc_serialize_float, \
	double*: fc_serialize_double, \
	fa_anim_curve_t*: fa_serialize_anim_curve \
)(_serializer, _property)

#elif PLATFORM_WINDOWS

#define fc_serialize(_serializer, _property) _Generic((_property), \
	int8_t*: fc_serialize_int8,	\
	int16_t*: fc_serialize_int16, \
	int32_t*: fc_serialize_int32, \
	int64_t*: fc_serialize_int64, \
	uint8_t*: fc_serialize_uint8,	\
	uint16_t*: fc_serialize_uint16, \
	uint32_t*: fc_serialize_uint32, \
	uint64_t*: fc_serialize_uint64, \
	float*: fc_serialize_float, \
	double*: fc_serialize_double, \
	fa_anim_curve_t*: fa_serialize_anim_curve, \
	enum fm_axis_t*: fc_serialize_uint32 \
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

// interface - use this + FUR_SER_ADD, FUR_SER_REM macros
typedef struct fc_serializer_t
{
	int32_t version;
	FILE* file;
	bool isWriting;
} fc_serializer_t;

void fc_serialize_load(const char* path, fc_serializer_t* outSerializer);
void fc_serialize_save(const char* path, fc_serializer_t* outSerializer);

// simple type serialization functions
void fc_serialize_int8(fc_serializer_t* pSerializer, int8_t* prop);
void fc_serialize_int16(fc_serializer_t* pSerializer, int16_t* prop);
void fc_serialize_int32(fc_serializer_t* pSerializer, int32_t* prop);
void fc_serialize_int64(fc_serializer_t* pSerializer, int64_t* prop);
void fc_serialize_uint8(fc_serializer_t* pSerializer, uint8_t* prop);
void fc_serialize_uint16(fc_serializer_t* pSerializer, uint16_t* prop);
void fc_serialize_uint32(fc_serializer_t* pSerializer, uint32_t* prop);
void fc_serialize_uint64(fc_serializer_t* pSerializer, uint64_t* prop);
void fc_serialize_float(fc_serializer_t* pSerializer, float* prop);
void fc_serialize_double(fc_serializer_t* pSerializer, double* prop);

void fc_serialize_buffer(fc_serializer_t* pSerializer, void* ptr, uint32_t size);

typedef struct fa_anim_curve_t fa_anim_curve_t;
void fa_serialize_anim_curve(fc_serializer_t* pSerializer, fa_anim_curve_t* animCurve);

#ifdef __cplusplus
}
#endif // __cplusplus
