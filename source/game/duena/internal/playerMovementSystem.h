#pragma once

namespace game
{
	// test code
	class PlayerMovementSystem : public IGameSystem
	{
	public:
		void AddPlayer(EntityID id);
		void RemoveAllPlayers();
		
		virtual void Tick(const TickSystemContext& ctx, float dt) override;
		
	private:
		static const uint32 NUM_MAX_PLAYERS = 16;
		EntityID m_entityIDs[NUM_MAX_PLAYERS];
		math::Vector2 m_velocities[NUM_MAX_PLAYERS];
		float m_shootingCooldown[NUM_MAX_PLAYERS];
		uint32 m_numPlayers = 0;
	};
}
