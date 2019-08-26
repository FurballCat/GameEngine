#include "pch.h"
#include "gameSpawners.h"
#include "cameraSystem.h"
#include "debugMeshRenderSystem.h"
#include "playerMovementSystem.h"
#include "projectileSystem.h"
#include "transformSystem.h"
#include "spriteSystem.h"

using namespace game;
using namespace math;

void SpawnPlayer::Func(void *ctx, const void *cmd)
{
	EntitySpawnContext* context = reinterpret_cast<EntitySpawnContext*>(ctx);
	const SpawnPlayer* data = reinterpret_cast<const SpawnPlayer*>(cmd);
	
	TransformSystem* transformSystem = context->GetSystem<TransformSystem>();
	PlayerMovementSystem* playerMovementSystem = context->GetSystem<PlayerMovementSystem>();
	CameraSystem* cameraSystem = context->GetSystem<CameraSystem>();
	DebugMeshRenderSystem* debugMeshRenderSystem = context->GetSystem<DebugMeshRenderSystem>();
	
	EntityID id = context->GetNewEntityID();
	math::Transform transform = math::CreateTransformIdentity();
	transform.translation = math::CreateVector4(data->m_position, 0.0f);
	
	transformSystem->CreateTransform(id, transform);
	playerMovementSystem->AddPlayer(id);
	cameraSystem->AddEntityToFollow(id);
	debugMeshRenderSystem->VisualizeEntity(id, 0.5f, {1, 1, 1});
}

void SpawnSimpleBullet::Func(void *ctx, const void *cmd)
{
	EntitySpawnContext* context = reinterpret_cast<EntitySpawnContext*>(ctx);
	const SpawnSimpleBullet* data = reinterpret_cast<const SpawnSimpleBullet*>(cmd);
	
	TransformSystem* transformSystem = context->GetSystem<TransformSystem>();
	SpriteSystem* spriteSystem = context->GetSystem<SpriteSystem>();
	ProjectileSystem* projectileSystem = context->GetSystem<ProjectileSystem>();
	
	EntityID id = context->GetNewEntityID();
	math::Transform transform = math::CreateTransformIdentity();
	transform.translation = math::CreateVector4(data->m_position, 0.0f);
	
	transformSystem->CreateTransform(id, transform);
	spriteSystem->CreateSprite(id, "rings", {-0.5f, -0.5f}, {0.2f, 0.2f}, {1.0f, 0.0f, 0.0f});
	projectileSystem->CreateProjectile(id, data->m_velocity);
}
