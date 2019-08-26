#pragma once

namespace input
{
    struct InputEvent;
	typedef uint32 InputEventID;
}

namespace rend
{
	struct ConstantBufferData;
}

namespace game
{
	constexpr uint32 TILE_LENGTH = 32;
	constexpr uint64 TILE_CHUNK_SIZE = TILE_LENGTH * TILE_LENGTH;
	constexpr uint64 GetTileIndex(uint32 x, uint32 y)
	{
		return x + y * TILE_LENGTH;
	}
}
