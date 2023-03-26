/* Copyright (c) Furball Cat */

#include "serialize.h"
#include "fileIO.h"

#define FUR_SIMPLE_TYPE_SERIALIZER_IMPL(_func, _type) \
void _func(fc_serializer_t* pSerializer, _type* prop) \
{ \
	if(!pSerializer->isWriting) \
	{ \
		fc_file_read(prop, sizeof(_type), 1, pSerializer->file); \
	} \
	else \
	{ \
		fc_file_write(prop, sizeof(_type), 1, pSerializer->file); \
	} \
}

FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fc_serialize_i8, i8 )
FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fc_serialize_i16, i16 )
FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fc_serialize_i32, i32 )
FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fc_serialize_i64, i64 )

FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fc_serialize_u8, u8 )
FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fc_serialize_u16, u16 )
FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fc_serialize_u32, u32 )
FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fc_serialize_u64, u64 )

FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fc_serialize_f32, f32 )
FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fc_serialize_f64, f64 )

void fc_serialize_buffer(fc_serializer_t* pSerializer, void* ptr, u32 size)
{
	if(!pSerializer->isWriting)
	{
		fc_file_read(ptr, size, 1, pSerializer->file);
	}
	else
	{
		fc_file_write(ptr, size, 1, pSerializer->file);
	}
}
