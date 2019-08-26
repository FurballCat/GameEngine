#include "pch.h"
#include "playerMovementSystem.h"
#include "transformSystem.h"
#include "gameActions.h"
#include "cameraSystem.h"
#include "gameSpawners.h"

using namespace game;
using namespace math;

void PlayerMovementSystem::AddPlayer(EntityID id)
{
	FUR_ASSERT(m_numPlayers < NUM_MAX_PLAYERS);
	const uint32 index = m_numPlayers;
	m_entityIDs[index] = id;
	m_velocities[index] = {0.0f, 0.0f};
	m_shootingCooldown[index] = 0.0f;
	++m_numPlayers;
}

void PlayerMovementSystem::RemoveAllPlayers()
{
	m_numPlayers = 0;
}

void PlayerMovementSystem::Tick(const TickSystemContext& ctx, float dt)
{
	for(uint32 playerID=0; playerID<m_numPlayers; ++playerID)
	{
		Vector2 moveDir;
		moveDir.x = -ctx.GetAction(playerID, GA_MoveLeft);
		moveDir.y = ctx.GetAction(playerID, GA_MoveUp);
		
		if(math::Mag(moveDir) < 0.08f)
			moveDir = {0.0f, 0.0f};
		
		static float s_maxSpeed = 4.0f;
		static float s_acceleration = 16.0f;
		static float s_friction = 2.0f;
		
		Vector2 velocity2D = m_velocities[playerID];
		
		Vector2 acceleration = s_acceleration * moveDir - velocity2D * s_friction;
		
		velocity2D = ClampMag(velocity2D + acceleration * dt, -s_maxSpeed, s_maxSpeed);
		
		m_velocities[playerID] = velocity2D;
		
		//CameraSystem* cameraSystem = ctx.GetSystem<CameraSystem>();
		
		//Vector4 eye = math::CreateVector4(cameraSystem->GetEye(), 0.0f);
		//Vector4 up = math::CreateVector4(cameraSystem->GetUp(), 0.0f);
		Vector4 rightAxis = {1.0f, -1.0f, 0.0f, 0.0f};
		Vector4 forwardAxis = {1.0f, 1.0f, 0.0f, 0.0f};
		
		rightAxis = math::Normalize(rightAxis);
		forwardAxis = math::Normalize(forwardAxis);
		
		TransformSystem* sysTransform = ctx.GetSystem<TransformSystem>();
		Transform& transform = sysTransform->GetTransformFor(m_entityIDs[playerID]);
		Vector4 velocity = rightAxis * velocity2D.x + forwardAxis * velocity2D.y;
		transform.translation += velocity * dt;
		
		// test code for shooting
		if(m_shootingCooldown[playerID] <= 0.0f)
		{
			if(ctx.GetAction(playerID, GA_Shoot))
			{
				SpawnSimpleBullet bullet;
				bullet.m_position = math::GetXYZ(transform.translation);
				bullet.m_position.z -= 0.1f;
				bullet.m_velocity = math::GetXYZ(velocity * 2.0f);
				
				ctx.Spawn(bullet);
				
				m_shootingCooldown[playerID] = 0.15f;
			}
		}
		else
		{
			m_shootingCooldown[playerID] -= dt;
		}
		//--
	}
}
