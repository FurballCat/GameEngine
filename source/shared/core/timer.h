#pragma once

class Timer
{
public:
	static Timer Tick();
	double Tock();
	double AbsoluteTime() const;
	
private:
	uint64 m_last;
	uint64 m_start;
	uint64 m_end;
	uint64 m_rate;
};

struct TimeInfo
{
	double t;	// absolute time
	float dt;	// delta time since last tick
};
