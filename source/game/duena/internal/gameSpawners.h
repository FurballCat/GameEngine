#pragma once

namespace game
{
	struct SpawnPlayer
	{
		math::Vector3 m_position;
		
		static void Func(void* ctx, const void* cmd);
	};
	
	struct SpawnSimpleBullet
	{
		math::Vector3 m_position;
		math::Vector3 m_velocity;
		
		static void Func(void* ctx, const void* cmd);
	};
}
