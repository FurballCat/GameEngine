#include "pch.h"
#include "audioEngine.h"

using namespace audio;

bool AudioEngine::Initialize()
{
	FMOD::System_Create(&m_system);
	m_system->init(128, FMOD_INIT_NORMAL, nullptr);
	
	m_system->createSound("../../assets/music_sails_on_the_horizon.wav", FMOD_DEFAULT, nullptr, &m_music);
	
	m_system->playSound(m_music, nullptr, false, &m_musicChannel);
	return true;
}

void AudioEngine::Update()
{
	if(m_musicChannel)
	{
		bool isPlaying = false;
		m_musicChannel->isPlaying(&isPlaying);
		if(isPlaying)
		{
			m_system->update();
		}
	}
}
