#include "unitTests/unitTestsFramework.h"

using namespace test;

typedef struct fc_bit_stream
{
	uint32_t* dataPtr;
	uint32_t numDataLeft;
	uint32_t bitPos;
	uint32_t bitrate;
} fc_bit_stream;

typedef struct fc_bit_stream_const
{
	const uint32_t* dataPtr;
	uint32_t numDataLeft;
	uint32_t bitPos;
	uint32_t bitrate;
} fc_bit_stream_const;

bool fc_bit_stream_write(fc_bit_stream* stream, uint32_t bits)
{
	// if no destination data available, exit
	if(stream->numDataLeft == 0 || (stream->numDataLeft == 1 && (stream->bitPos + stream->bitrate) > 32))
		return false;
	
	// cache 2x 32 bits
	uint64_t data = stream->dataPtr[0];
	if(stream->numDataLeft > 1)
		data = ((uint64_t)(stream->dataPtr[1]) << 32) | data;
	
	// clear bits that we're about to write to
	data = data & (~1llu >> stream->bitPos);
	
	// write to 64 bit cache
	data = data | (((uint64_t)bits) << stream->bitPos);
	
	// copy back to 2x 32 bits
	stream->dataPtr[0] = (uint32_t)(data);
	if(stream->numDataLeft > 1)
		stream->dataPtr[1] = (uint32_t)(data >> 32);
	
	// advance stream
	stream->bitPos += stream->bitrate;
	if(stream->bitPos > 32)
	{
		stream->bitPos = stream->bitPos % 32;
		stream->dataPtr++;
		stream->numDataLeft--;
	}
	
	return true;
}

bool fc_bit_stream_read(fc_bit_stream_const* stream, uint32_t* bits)
{
	// if no source data available, exit
	if(stream->numDataLeft == 0 || (stream->numDataLeft == 1 && (stream->bitPos + stream->bitrate) > 32))
		return false;
	
	// cache 2x 32 bits
	uint64_t data = stream->dataPtr[0];
	if(stream->numDataLeft > 1)
		data = ((uint64_t)(stream->dataPtr[1]) << 32) | data;
	
	// read bits
	*bits = (uint32_t)((data << (64 - stream->bitPos - stream->bitrate)) >> (64 - stream->bitrate));
	
	// advance stream
	stream->bitPos += stream->bitrate;
	if(stream->bitPos > 32)
	{
		stream->bitPos = stream->bitPos % 32;
		stream->dataPtr++;
		stream->numDataLeft--;
	}
	
	return true;
}

typedef struct fa_quant_compression_clip_extents
{
	float minimum;
	float extent;
} fa_quant_compression_clip_extents;

typedef struct fa_quant_compression_bucket_extents
{
	float minimum;
	float extent;
	uint8_t bitrate;
} fa_quant_compression_bucket_extents;

void fa_quant_compression_compress_single(float clipMin, float clipExtent, float bucketMin, float bucketExtent, float value, uint32_t maxInt, uint32_t* outValue)
{
	const float normValue = (value - clipMin) / clipExtent;
	const float normValue2 = (normValue - bucketMin) / bucketExtent;
	*outValue = normValue2 * maxInt;
}

void fa_quant_compression_decompress_single(float clipMin, float clipExtent, float bucketMin, float bucketExtent, uint32_t value, uint32_t maxInt, float* outValue)
{
	const float normValue2 = (float)value / (float)maxInt;
	const float normValue = normValue2 * bucketExtent + bucketMin;
	*outValue = normValue * clipExtent + clipMin;
}

void fa_quant_compression_get_min_max(const float* keyData, uint32_t numKeyData, float* outMinimum, float* outMaximum)
{
	if(numKeyData == 0)
		return;
	
	float minimum = keyData[0];
	float maximum = keyData[0];
	
	for(uint32_t i=1; i<numKeyData; ++i)
	{
		if(keyData[i] < minimum)
			minimum = keyData[i];
		
		if(keyData[i] > maximum)
			maximum = keyData[i];
	}
	
	*outMinimum = minimum;
	*outMaximum = maximum;
}

void fa_quant_compression_calc_clip_extents(fa_quant_compression_clip_extents* opts, const float* keyData, uint32_t numKeyData)
{
	float minimum = 0.0f;
	float maximum = 0.0f;
	
	fa_quant_compression_get_min_max(keyData, numKeyData, &minimum, &maximum);
	
	opts->minimum = minimum;
	opts->extent = maximum - minimum;
}

void fa_quant_compression_calc_bucket_extents(fa_quant_compression_clip_extents clipExtents,
											  fa_quant_compression_bucket_extents* bucketOpts,
											  const float* keyData, uint32_t numKeyData)
{
	float minimum = 0.0f;
	float maximum = 0.0f;
	
	fa_quant_compression_get_min_max(keyData, numKeyData, &minimum, &maximum);
	
	bucketOpts->minimum = (minimum - clipExtents.minimum) / clipExtents.extent;
	bucketOpts->extent = (maximum - minimum) / clipExtents.extent;
}

void fa_quant_compress(fa_quant_compression_clip_extents clipExtents, fa_quant_compression_bucket_extents bucketExtents,
								   const float* data, uint32_t numData, uint32_t* outData)
{
	const uint32_t intMax = (1 << bucketExtents.bitrate) - 1;
	
	fc_bit_stream bitStream = {};
	bitStream.bitrate = bucketExtents.bitrate;
	bitStream.dataPtr = outData;
	bitStream.numDataLeft = numData * bucketExtents.bitrate / 32;
	
	uint32_t value = 0;
	for(uint32_t i=0; i<numData; ++i)
	{
		fa_quant_compression_compress_single(clipExtents.minimum, clipExtents.extent, bucketExtents.minimum, bucketExtents.extent, data[i], intMax, &value);
		
		fc_bit_stream_write(&bitStream, value);
	}
}

void fa_quant_decompress(fa_quant_compression_clip_extents clipExtents, fa_quant_compression_bucket_extents bucketExtents,
								   const uint32_t* data, uint32_t numOutData, float* outData)
{
	const uint32_t intMax = (1 << bucketExtents.bitrate) - 1;
	
	fc_bit_stream_const bitStream = {};
	bitStream.bitrate = bucketExtents.bitrate;
	bitStream.dataPtr = data;
	bitStream.numDataLeft = numOutData * bucketExtents.bitrate / 32;
	
	uint32_t value;
	for(uint32_t i=0; i<numOutData; ++i)
	{
		fc_bit_stream_read(&bitStream, &value);
		fa_quant_compression_decompress_single(clipExtents.minimum, clipExtents.extent, bucketExtents.minimum, bucketExtents.extent, value, intMax, &outData[i]);
	}
}

UNITTEST(AnimQuantCompression, simple_1)
{
	const float clipMin = 0.0f;
	const float clipExtent = 4.0f;
	const float bucketMin = 0.2f;
	const float bucketExtent = 0.6f;
	const uint32_t maxInt = 1 << 12;
	
	const float valueIn = 2.1f;
	uint32_t valueCompressed = 0;
	float valueOut = 0.0f;
	
	fa_quant_compression_compress_single(clipMin, clipExtent, bucketMin, bucketExtent, valueIn, maxInt, &valueCompressed);
	fa_quant_compression_decompress_single(clipMin, clipExtent, bucketMin, bucketExtent, valueCompressed, maxInt, &valueOut);
	
	return Assert::AreEqual(valueIn, valueOut, 0.001f);
}

UNITTEST(AnimQuantCompression, simple_2)
{
	const float clipMin = 0.0f;
	const float clipExtent = 4.0f;
	const float bucketMin = 0.2f;
	const float bucketExtent = 0.6f;
	const uint32_t maxInt = 1 << 8;
	
	const float valueIn = 2.1f;
	uint32_t valueCompressed = 0;
	float valueOut = 0.0f;
	
	fa_quant_compression_compress_single(clipMin, clipExtent, bucketMin, bucketExtent, valueIn, maxInt, &valueCompressed);
	fa_quant_compression_decompress_single(clipMin, clipExtent, bucketMin, bucketExtent, valueCompressed, maxInt, &valueOut);
	
	return Assert::AreEqual(valueIn, valueOut, 0.01f);
}

UNITTEST(AnimQuantCompression, simple_3)
{
	const float clipMin = 0.0f;
	const float clipExtent = 4.0f;
	const float bucketMin = 0.2f;
	const float bucketExtent = 0.6f;
	const uint32_t maxInt = 1 << 4;
	
	const float valueIn = 2.1f;
	uint32_t valueCompressed = 0;
	float valueOut = 0.0f;
	
	fa_quant_compression_compress_single(clipMin, clipExtent, bucketMin, bucketExtent, valueIn, maxInt, &valueCompressed);
	fa_quant_compression_decompress_single(clipMin, clipExtent, bucketMin, bucketExtent, valueCompressed, maxInt, &valueOut);
	
	Assert::AreEqual(valueIn, valueOut, 0.1f);
}

UNITTEST(AnimQuantCompression, calc_clip_extents_1)
{
	fa_quant_compression_clip_extents opts = {};
	
	const float keyData[] = {-2.0f, 2.0f};
	const uint32_t numKeyData = sizeof(keyData) / sizeof(float);
	
	fa_quant_compression_calc_clip_extents(&opts, keyData, numKeyData);
	
	Assert::AreEqual(opts.minimum, -2.0f);
	Assert::AreEqual(opts.extent, 4.0f);
}

UNITTEST(AnimQuantCompression, calc_clip_extents_2)
{
	fa_quant_compression_clip_extents opts = {};
	
	const float keyData[] = {-2.0f, 2.0f, -4.2f, 5.3f};
	const uint32_t numKeyData = sizeof(keyData) / sizeof(float);
	
	fa_quant_compression_calc_clip_extents(&opts, keyData, numKeyData);
	
	Assert::AreEqual(opts.minimum, -4.2f);
	Assert::AreEqual(opts.extent, 9.5f);
}

UNITTEST(AnimQuantCompression, calc_bucket_extents_1)
{
	fa_quant_compression_clip_extents opts = {};
	
	const float keyData[] = {-4.0f, -2.0f, 2.0f, 4.0f};
	const uint32_t numKeyData = sizeof(keyData) / sizeof(float);
	
	fa_quant_compression_calc_clip_extents(&opts, keyData, numKeyData);
	
	fa_quant_compression_bucket_extents bucketOpts = {};
	fa_quant_compression_calc_bucket_extents(opts, &bucketOpts, keyData + 1, 2);
	
	Assert::AreEqual(bucketOpts.minimum, 0.25f);
	Assert::AreEqual(bucketOpts.extent, 0.5f);
}

UNITTEST(BitStream, write_1)
{
	uint32_t data = 0;
	
	fc_bit_stream stream = {};
	stream.bitrate = 8;
	stream.dataPtr = &data;
	stream.numDataLeft = 1;
	
	fc_bit_stream_write(&stream, 0xFF);
	
	Assert::AreEqual(data, 0x000000FF);
}

UNITTEST(BitStream, write_2)
{
	uint32_t data = 0;
	
	fc_bit_stream stream = {};
	stream.bitrate = 8;
	stream.dataPtr = &data;
	stream.numDataLeft = 1;
	
	fc_bit_stream_write(&stream, 0xFF);
	fc_bit_stream_write(&stream, 0xFF);
	
	Assert::AreEqual(data, 0x0000FFFF);
}

UNITTEST(BitStream, write_3)
{
	uint32_t data = 0;
	
	fc_bit_stream stream = {};
	stream.bitrate = 8;
	stream.dataPtr = &data;
	stream.numDataLeft = 1;
	
	fc_bit_stream_write(&stream, 0xFF);
	fc_bit_stream_write(&stream, 0xCC);
	fc_bit_stream_write(&stream, 0x00);
	fc_bit_stream_write(&stream, 0xAA);
	
	Assert::AreEqual(data, 0xAA00CCFF);
}

UNITTEST(BitStream, write_4)
{
	uint32_t data[2] = {};
	
	fc_bit_stream stream = {};
	stream.bitrate = 8;
	stream.dataPtr = data;
	stream.numDataLeft = sizeof(data) / sizeof(uint32_t);
	
	fc_bit_stream_write(&stream, 0xFF);
	fc_bit_stream_write(&stream, 0xCC);
	fc_bit_stream_write(&stream, 0x00);
	fc_bit_stream_write(&stream, 0xAA);
	fc_bit_stream_write(&stream, 0x11);
	fc_bit_stream_write(&stream, 0x22);
	fc_bit_stream_write(&stream, 0x33);
	fc_bit_stream_write(&stream, 0x44);
	
	Assert::AreEqual(data[0], 0xAA00CCFF);
	Assert::AreEqual(data[1], 0x44332211);
}

UNITTEST(BitStream, write_5)
{
	const uint32_t entries[] = {0x1, 0x2, 0x3, 0xA, 0xB, 0xC, 0xD, 0xE, 0xF};
	const uint32_t numEntries = sizeof(entries) / sizeof(uint32_t);
	
	uint32_t data[2] = {};
	
	fc_bit_stream stream = {};
	stream.bitrate = 5;
	stream.dataPtr = data;
	stream.numDataLeft = sizeof(data) / sizeof(uint32_t);
	
	for(uint32_t i=0; i<numEntries; ++i)
		fc_bit_stream_write(&stream, entries[i]);
	
	fc_bit_stream_const streamRead = {};
	streamRead.bitrate = 5;
	streamRead.dataPtr = data;
	streamRead.numDataLeft = sizeof(data) / sizeof(uint32_t);
	
	for(uint32_t i=0; i<numEntries; ++i)
	{
		uint32_t value = 0;
		fc_bit_stream_read(&streamRead, &value);
		Assert::AreEqual(value, entries[i]);
	}
	
}

UNITTEST(BitStream, read_1)
{
	const uint32_t data = 0xAA00CCFF;
	
	fc_bit_stream_const stream = {};
	stream.bitrate = 8;
	stream.dataPtr = &data;
	stream.numDataLeft = 1;
	
	uint32_t outData = 0;
	fc_bit_stream_read(&stream, &outData);
	Assert::AreEqual(outData, 0xFF);
	
	outData = 0;
	fc_bit_stream_read(&stream, &outData);
	Assert::AreEqual(outData, 0xCC);
	
	outData = 0xFF;
	fc_bit_stream_read(&stream, &outData);
	Assert::AreEqual(outData, 0x00);
	
	outData = 0;
	fc_bit_stream_read(&stream, &outData);
	Assert::AreEqual(outData, 0xAA);
}

UNITTEST(BitStream, read_2)
{
	const uint32_t data[2] = {0xAA00CCFF, 0x44332211};
	
	fc_bit_stream_const stream = {};
	stream.bitrate = 8;
	stream.dataPtr = data;
	stream.numDataLeft = sizeof(data) / sizeof(uint32_t);
	
	uint32_t outData = 0;
	fc_bit_stream_read(&stream, &outData);
	Assert::AreEqual(outData, 0xFF);
	
	outData = 0;
	fc_bit_stream_read(&stream, &outData);
	Assert::AreEqual(outData, 0xCC);
	
	outData = 0xFF;
	fc_bit_stream_read(&stream, &outData);
	Assert::AreEqual(outData, 0x00);
	
	outData = 0;
	fc_bit_stream_read(&stream, &outData);
	Assert::AreEqual(outData, 0xAA);
	
	outData = 0;
	fc_bit_stream_read(&stream, &outData);
	Assert::AreEqual(outData, 0x11);
	
	outData = 0;
	fc_bit_stream_read(&stream, &outData);
	Assert::AreEqual(outData, 0x22);
	
	outData = 0;
	fc_bit_stream_read(&stream, &outData);
	Assert::AreEqual(outData, 0x33);
	
	outData = 0;
	fc_bit_stream_read(&stream, &outData);
	Assert::AreEqual(outData, 0x44);
}

UNITTEST(AnimQuantCompression, compress_data_1)
{
	const float keyData[] = {-4.0f, -2.0f, 2.0f, 4.0f};
	const uint32_t numKeyData = sizeof(keyData) / sizeof(float);
	
	fa_quant_compression_clip_extents clip = {};
	fa_quant_compression_bucket_extents bucket = {};
	fa_quant_compression_calc_clip_extents(&clip, keyData, numKeyData);
	fa_quant_compression_calc_bucket_extents(clip, &bucket, keyData, numKeyData);
	
	bucket.bitrate = 8;
	
	uint32_t dataCompressed = 0;
	
	fa_quant_compress(clip, bucket, keyData, numKeyData, &dataCompressed);
	
	float decompressedKeys[numKeyData] = {};
	fa_quant_decompress(clip, bucket, &dataCompressed, numKeyData, decompressedKeys);
	
	for(uint32_t i=0; i<numKeyData; ++i)
	{
		Assert::AreEqual(keyData[i], decompressedKeys[i], 0.1f);
	}
}

UNITTEST(AnimQuantCompression, compress_data_2)
{
	const float keyData[] = {8.0f, 20.0f, 2.0f, 14.0f, -8.0f, -5.0f, -2.0f, -4.0f, 8.0f, 7.0f, 6.0f, 7.2f, 8.0f, 20.0f, 2.0f, 14.0f};
	const uint32_t numKeyData = sizeof(keyData) / sizeof(float);
	
	fa_quant_compression_clip_extents clip = {};
	fa_quant_compression_bucket_extents bucket = {};
	fa_quant_compression_calc_clip_extents(&clip, keyData, numKeyData);
	fa_quant_compression_calc_bucket_extents(clip, &bucket, keyData, numKeyData);
	
	bucket.bitrate = 16;
	
	uint32_t dataCompressed[8] = {};
	
	fa_quant_compress(clip, bucket, keyData, numKeyData, dataCompressed);
	
	float decompressedKeys[numKeyData] = {};
	fa_quant_decompress(clip, bucket, dataCompressed, numKeyData, decompressedKeys);
	
	for(uint32_t i=0; i<numKeyData; ++i)
	{
		Assert::AreEqual(keyData[i], decompressedKeys[i], 0.1f);
	}
}

