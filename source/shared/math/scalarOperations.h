#pragma once
#include "api.h"
#include "types.h"
#include <math.h>

namespace math
{
    inline float Sin(float x) { return ::sin(x); }
    inline float Cos(float x) { return ::cos(x); }
    inline float Tan(float x) { return ::tan(x); }
    inline float ASin(float x) { return ::asin(x); }
    inline float ACos(float x) { return ::acos(x); }
    inline float ATan(float x) { return ::atan(x); }
	
	template <typename T>
	constexpr T Min(T x, T y) { return x < y ? x : y; }
	
	template <typename T>
	constexpr T Max(T x, T y) { return x > y ? x : y; }
    
    inline float Sqrt(float x) { return ::sqrt(x); }
	inline float Pow(float x, float y) { return ::pow(x, y); }
    
    constexpr double operator"" _deg(long double degrees)
    {
        return degrees * c_pi / 180.0;
    }
    
    constexpr double operator"" _deg(uint64 degrees)
    {
        return static_cast<long double>(degrees) * c_pi / 180.0;
    }
	
	inline float Clamp(float x, float min, float max)
	{
		if(x < min)
			return min;
		
		if(x > max)
			return max;
		
		return x;
	}
	
	inline uint32 Clamp(uint32 x, uint32 min, uint32 max)
	{
		if(x < min)
			return min;
		
		if(x > max)
			return max;
		
		return x;
	}
	
	constexpr float Lerp(float x, float y, float alpha)
	{
		return x * (1.0f - alpha) + y * alpha;
	}
}
