/* Copyright (c) 2016-2021 Furball Cat */

#include "serialize.h"
#include <string.h>
#include <stdio.h>

#define FUR_SIMPLE_TYPE_SERIALIZER_IMPL(_func, _type) \
void _func(fc_serializer_t* pSerializer, _type* prop) \
{ \
	if(!pSerializer->isWriting) \
	{ \
		fread(prop, sizeof(_type), 1, pSerializer->file); \
	} \
	else \
	{ \
		fwrite(prop, sizeof(_type), 1, pSerializer->file); \
	} \
}

FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fc_serialize_int8, int8_t )
FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fc_serialize_int16, int16_t )
FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fc_serialize_int32, int32_t )
FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fc_serialize_int64, int64_t )

FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fc_serialize_uint8, uint8_t )
FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fc_serialize_uint16, uint16_t )
FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fc_serialize_uint32, uint32_t )
FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fc_serialize_uint64, uint64_t )

FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fc_serialize_float, float )
FUR_SIMPLE_TYPE_SERIALIZER_IMPL( fc_serialize_double, double )
