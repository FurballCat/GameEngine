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

#define fcSerialize(_serializer, _property) _Generic((_property), \
	int8_t*: fcSerializeInt8,	\
	int16_t*: fcSerializeInt16, \
	i32*: fcSerializeInt32, \
	int64_t*: fcSerializeInt64, \
	u8*: fcSerializeUint8,	\
	u16*: fcSerializeUint16, \
	u32*: fcSerializeUint32, \
	uint64_t*: fcSerializeUint64, \
	f32*: fcSerializeFloat32, \
	f64*: fcSerializeFloat64, \
	FcAnimCurve*: fcSerializeAnimCurve, \
)(_serializer, _property)

#elif PLATFORM_WINDOWS

#define fcSerialize(_serializer, _property) _Generic((_property), \
	int8_t*: fcSerializeInt8,	\
	int16_t*: fcSerializeInt16, \
	i32*: fcSerializeInt32, \
	int64_t*: fcSerializeInt64, \
	u8*: fcSerializeUint8,	\
	u16*: fcSerializeUint16, \
	u32*: fcSerializeUint32, \
	uint64_t*: fcSerializeUint64, \
	f32*: fcSerializeFloat32, \
	f64*: fcSerializeFloat64, \
	FcAnimCurve*: fcSerializeAnimCurve, \
	enum fm_axis_t*: fcSerializeUint32 \
)(_serializer, _property)

#endif

#define FUR_SER_VERSION(_versionLatest)	\
	if(pSerializer->isWriting) \
	{ \
		pSerializer->version = _versionLatest;	\
		fcSerialize(pSerializer, &pSerializer->version); \
	} \
	else \
	{ \
		fcSerialize(pSerializer, &pSerializer->version); \
	}

#define FUR_SER_ADD(_versionAdded, _property) \
	if(pSerializer->version >= _versionAdded) \
	{\
		fcSerialize(pSerializer, &_property);\
	}

#define FUR_SER_ADD_BUFFER(_versionAdded, _ptr, _sizeInBytes) \
	if(pSerializer->version >= _versionAdded) \
	{\
		fcSerializeBuffer(pSerializer, _ptr, _sizeInBytes);\
	}

#define FUR_SER_REM(_versionAdded, _versionRemoved, _type, _property, _defaultValue) \
	_type _property = _defaultValue; \
	if(pSerializer->version >= _versionAdded && pSerializer->version < _versionRemoved)	\
	{	\
		fcSerialize(pSerializer, &_property); \
	}

#ifdef PLATFORM_OSX
typedef struct __sFILE FILE;
#elif PLATFORM_WINDOWS
typedef struct _iobuf FILE;
#else
#endif

typedef struct FcDepot FcDepot;
typedef u64 FcFilePath;
typedef struct FcFile FcFile;

// interface - use this + FUR_SER_ADD, FUR_SER_REM macros
typedef struct FcSerializer
{
	i32 version;
	FcFile* file;
	bool isWriting;
} FcSerializer;

// simple type serialization functions
void fcSerializeInt8(FcSerializer* pSerializer, i8* prop);
void fcSerializeInt16(FcSerializer* pSerializer, i16* prop);
void fcSerializeInt32(FcSerializer* pSerializer, i32* prop);
void fcSerializeInt64(FcSerializer* pSerializer, i64* prop);
void fcSerializeUint8(FcSerializer* pSerializer, u8* prop);
void fcSerializeUint16(FcSerializer* pSerializer, u16* prop);
void fcSerializeUint32(FcSerializer* pSerializer, u32* prop);
void fcSerializeUint64(FcSerializer* pSerializer, u64* prop);
void fcSerializeFloat32(FcSerializer* pSerializer, f32* prop);
void fcSerializeFloat64(FcSerializer* pSerializer, f64* prop);

void fcSerializeBuffer(FcSerializer* pSerializer, void* ptr, u32 size);

typedef struct FcAnimCurve FcAnimCurve;
void fcSerializeAnimCurve(FcSerializer* pSerializer, FcAnimCurve* animCurve);

#ifdef __cplusplus
}
#endif // __cplusplus
