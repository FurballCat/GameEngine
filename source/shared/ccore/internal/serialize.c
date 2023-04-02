/* Copyright (c) Furball Cat */

#include "serialize.h"
#include "fileIO.h"

#define FUR_SIMPLE_TYPE_SERIALIZER_IMPL(_func, _type) \
void _func(FcSerializer* pSerializer, _type* prop) \
{ \
	if(!pSerializer->isWriting) \
	{ \
		fcFileRead(prop, sizeof(_type), 1, pSerializer->file); \
	} \
	else \
	{ \
		fcFileWrite(prop, sizeof(_type), 1, pSerializer->file); \
	} \
}

FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fcSerializeInt8, i8 )
FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fcSerializeInt16, i16 )
FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fcSerializeInt32, i32 )
FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fcSerializeInt64, i64 )

FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fcSerializeUint8, u8 )
FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fcSerializeUint16, u16 )
FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fcSerializeUint32, u32 )
FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fcSerializeUint64, u64 )

FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fcSerializeFloat32, f32 )
FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fcSerializeFloat64, f64 )

void fcSerializeBuffer(FcSerializer* pSerializer, void* ptr, u32 size)
{
	if(!pSerializer->isWriting)
	{
		fcFileRead(ptr, size, 1, pSerializer->file);
	}
	else
	{
		fcFileWrite(ptr, size, 1, pSerializer->file);
	}
}
