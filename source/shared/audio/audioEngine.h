#pragma once

namespace FMOD
{
	class System;
	class Sound;
	class Channel;
}

namespace audio
{
	class AUDIO_API AudioEngine
	{
	public:
		bool Initialize();
		
		void Update();
		
	private:
		FMOD::System* m_system = nullptr;
		
		FMOD::Sound* m_music = nullptr;
		
		FMOD::Channel* m_musicChannel = nullptr;
	};
}
