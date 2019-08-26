#include "pch.h"
#include "gameSystem.h"

using namespace game;

EntitySpawnQueue::EntitySpawnQueue(fur::CommandEncoder commandEncoder)
	: m_commandEncoder(commandEncoder)
{
	
}

GAMEWORK_API uint32 game::NewGameSystemID()
{
	static uint32 id = 0;
	return id++;
}
