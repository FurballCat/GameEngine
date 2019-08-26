#pragma once
#include "api.h"
#include "types.h"
#include "scalarOperations.h"

namespace math
{
	// Vector2 operations
	
	inline Vector2 Add(const Vector2 MREF a, const Vector2 MREF b)				{ return {a.x + b.x, a.y + b.y}; }
	inline Vector2 operator+(const Vector2 MREF a, const Vector2 MREF b)		{ return {a.x + b.x, a.y + b.y}; }
	inline Vector2& operator+=(Vector2& a, const Vector2 MREF b)				{ a.x += b.x; a.y += b.y; return a; }
	
	inline Vector2 Sub(const Vector2 MREF a, const Vector2 MREF b)				{ return {a.x - b.x, a.y - b.y}; }
	inline Vector2 operator-(const Vector2 MREF a, const Vector2 MREF b)		{ return {a.x - b.x, a.y - b.y}; }
	inline Vector2& operator-=(Vector2& a, const Vector2 MREF b)				{ a.x -= b.x; a.y -= b.y; return a; }
	
	inline Vector2 Div(const Vector2 MREF a, const Vector2 MREF b)				{ return {a.x / b.x, a.y / b.y}; }
	inline Vector2 operator/(const Vector2 MREF a, const Vector2 MREF b)		{ return {a.x / b.x, a.y / b.y}; }
	inline Vector2& operator/=(Vector2& a, const Vector2 MREF b)				{ a.x /= b.x; a.y /= b.y; return a; }
	
	inline Vector2 Mul(const Vector2 MREF a, const Vector2 MREF b)				{ return {a.x * b.x, a.y * b.y}; }
	inline Vector2 operator*(const Vector2 MREF a, const Vector2 MREF b)		{ return {a.x * b.x, a.y * b.y}; }
	inline Vector2& operator*=(Vector2& a, const Vector2 MREF b)				{ a.x *= b.x; a.y *= b.y; return a; }
	
	inline Vector2 Mul(const Vector2 MREF a, const float b)						{ return {a.x * b, a.y * b}; }
	inline Vector2 operator*(const Vector2 MREF a, const float b)				{ return {a.x * b, a.y * b}; }
	inline Vector2& operator*=(Vector2& a, const float b)						{ a.x *= b; a.y *= b; return a; }
	
	inline Vector2 Mul(const float b, const Vector2 MREF a)						{ return {a.x * b, a.y * b}; }
	inline Vector2 operator*(const float b, const Vector2 MREF a)				{ return {a.x * b, a.y * b}; }
	
	inline Vector2& operator/=(Vector2& a, const float s)						{ a.x /= s; a.y /= s; return a; }
	
	inline float Mag(const Vector2 MREF v)										{ return Sqrt(v.x * v.x + v.y * v.y); }
	inline float Mag2(const Vector2 MREF v)										{ return v.x * v.x + v.y * v.y; }
	
	inline Vector2 Normalize(const Vector2 MREF v)
	{
		float magnitude = Mag(v);
		float magnitudeInv = 1.0f / magnitude;
		return {v.x * magnitudeInv, v.y * magnitudeInv};
	}
	
	inline Vector2 ClampMag(const Vector2 MREF a, float min, float max)
	{
		float magnitude = Mag(a);
		if(magnitude > 0)
		{
			Vector2 dir = Normalize(a);
			float clampedMagnitude = Clamp(magnitude, min, max);
			return clampedMagnitude * dir;
		}
		
		return a;
	}
	
	// Vector3 operations
	
	inline Vector3 operator+(const Vector3 MREF a, const Vector3 MREF b)		{ return {a.x + b.x, a.y + b.y, a.z + b.z}; }
	inline Vector3 Add(const Vector3 MREF a, const Vector3 MREF b)				{ return {a.x + b.x, a.y + b.y, a.z + b.z}; }
	inline Vector3& operator+=(Vector3& a, const Vector3 MREF b)				{ a.x += b.x; a.y += b.y; a.z += b.z; return a; }
	
	
	inline Vector3 operator-(const Vector3 MREF a, const Vector3 MREF b)		{ return {a.x - b.x, a.y - b.y, a.z - b.z}; }
	inline Vector3 Sub(const Vector3 MREF a, const Vector3 MREF b)				{ return {a.x - b.x, a.y - b.y, a.z - b.z}; }
	inline Vector3& operator-=(Vector3& a, const Vector3 MREF b)				{ a.x -= b.x; a.y -= b.y; a.z -= b.z; return a; }
	
	inline Vector3 operator-(const Vector3 MREF a)								{ return {-a.x, -a.y, -a.z}; }
	
	inline Vector3 Div(const Vector3 MREF a, const Vector3 MREF b)				{ return {a.x / b.x, a.y / b.y, a.z / b.z}; }
	inline Vector3 operator/(const Vector3 MREF a, const Vector3 MREF b)		{ return {a.x / b.x, a.y / b.y, a.z / b.z}; }
	inline Vector3& operator/=(Vector3& a, const Vector3 MREF b)				{ a.x /= b.x; a.y /= b.y; a.z /= b.z; return a; }
	
	inline Vector3 Mul(const Vector3 MREF a, const Vector3 MREF b)				{ return {a.x * b.x, a.y * b.y, a.z * b.z}; }
	inline Vector3 operator*(const Vector3 MREF a, const Vector3 MREF b)		{ return {a.x * b.x, a.y * b.y, a.z * b.z}; }
	inline Vector3& operator*=(Vector3& a, const Vector3 MREF b)				{ a.x *= b.x; a.y *= b.y; a.z *= b.z; return a; }
	
	inline Vector3 Mul(const Vector3 MREF a, const float b)						{ return {a.x * b, a.y * b, a.z * b}; }
	inline Vector3 operator*(const Vector3 MREF a, const float b)				{ return {a.x * b, a.y * b, a.z * b}; }
	inline Vector3& operator*=(Vector3& a, const float s)						{ a.x *= s; a.y *= s; a.z *= s; return a; }
	
	inline Vector3 Mul(const float b, const Vector3 MREF a)						{ return {a.x * b, a.y * b, a.z * b}; }
	inline Vector3 operator*(const float b, const Vector3 MREF a)				{ return {a.x * b, a.y * b, a.z * b}; }
	
	inline Vector3& operator/=(Vector3& a, const float s)						{ a.x /= s; a.y /= s; a.z /= s; return a; }
	
	inline float Mag(const Vector3 MREF v)										{ return Sqrt(v.x * v.x + v.y * v.y + v.z * v.z); }
	inline float Mag2(const Vector3 MREF v)										{ return v.x * v.x + v.y * v.y + v.z * v.z; }
	
	inline Vector3 Cross(const Vector3 MREF a, const Vector3 MREF b)			{ return {a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x}; }
	inline float Dot(const Vector3 MREF a, const Vector3 MREF b)				{ return a.x * b.x + a.y * b.y + a.z * b.z; }
	
	inline Vector3 Normalize(const Vector3 MREF v)
	{
		float magnitude = Mag(v);
		float magnitudeInv = 1.0f / magnitude;
		return {v.x * magnitudeInv, v.y * magnitudeInv, v.z * magnitudeInv};
	}
	
	// Vector4 operations
	
	constexpr Vector4 CreateVectorZeros()										{ return {0.0f, 0.0f, 0.0f, 0.0f}; }
	constexpr Vector4 CreateVectorOnes()										{ return {1.0f, 1.0f, 1.0f, 1.0f}; }
	
	inline Vector4 Add(const Vector4 MREF a, const Vector4 MREF b)				{ return {a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w}; }
    inline Vector4 operator+(const Vector4 MREF a, const Vector4 MREF b)		{ return {a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w}; }
	inline Vector4& operator+=(Vector4& a, const Vector4 MREF b)				{ a.x += b.x; a.y += b.y; a.z += b.z; a.w += b.w; return a; }
	
	inline Vector4 Sub(const Vector4 MREF a, const Vector4 MREF b)				{ return {a.x - b.x, a.y - b.y, a.z - b.z, a.w - b.w}; }
    inline Vector4 operator-(const Vector4 MREF a, const Vector4 MREF b)		{ return {a.x - b.x, a.y - b.y, a.z - b.z, a.w - b.w}; }
	inline Vector4& operator-=(Vector4& a, const Vector4 MREF b)				{ a.x -= b.x; a.y -= b.y; a.z -= b.z; a.w -= b.w; return a; }
	
	inline Vector4 operator-(const Vector4 MREF a)								{ return {-a.x, -a.y, -a.z, -a.w}; }
	
	inline Vector4 Mul(const Vector4 MREF a, const Vector4 MREF b)				{ return {a.x * b.x, a.y * b.y, a.z * b.z, a.w * b.w}; }
    inline Vector4 operator*(const Vector4 MREF a, const Vector4 MREF b)		{ return {a.x * b.x, a.y * b.y, a.z * b.z, a.w * b.w}; }
	inline Vector4& operator*=(Vector4& a, const Vector4 MREF b)				{ a.x *= b.x; a.y *= b.y; a.z *= b.z; a.w *= b.w; return a; }
	
	inline Vector4 Div(const Vector4 MREF a, const Vector4 MREF b)				{ return {a.x / b.x, a.y / b.y, a.z / b.z, a.w / b.w}; }
    inline Vector4 operator/(const Vector4 MREF a, const Vector4 MREF b)		{ return {a.x / b.x, a.y / b.y, a.z / b.z, a.w / b.w}; }
	inline Vector4& operator/=(Vector4& a, const Vector4 MREF b)				{ a.x /= b.x; a.y /= b.y; a.z /= b.z; a.w /= b.w; return a; }
    
    inline Vector4 Mul(const Vector4 MREF v, const float s)						{ return {v.x * s, v.y * s, v.z * s, v.w * s}; }
    inline Vector4 operator*(const Vector4 MREF v, const float s)				{ return {v.x * s, v.y * s, v.z * s, v.w * s}; }
	inline Vector4& operator*=(Vector4& a, const float s)						{ a.x *= s; a.y *= s; a.z *= s; a.w *= s; return a; }
    
    inline Vector4 Mul(const float s, const Vector4 MREF v)						{ return {v.x * s, v.y * s, v.z * s, v.w * s}; }
    inline Vector4 operator*(const float s, const Vector4 MREF v)				{ return {v.x * s, v.y * s, v.z * s, v.w * s}; }
	
	inline Vector4& operator/=(Vector4& a, const float s)						{ a.x /= s; a.y /= s; a.z /= s; a.w /= s; return a; }
	
    // note that there's no cross product of 4D vectors (only of 3D and 7D), so 'w' component will be always set to 0
    inline Vector4 Cross(const Vector4 MREF a, const Vector4 MREF b)			{ return {a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x, 0.0f}; }
	
	inline float Dot(const Vector4 MREF a, const Vector4 MREF b)				{ return a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w; }
    
    inline float Mag(const Vector4 MREF v)										{ return Sqrt(v.x * v.x + v.y * v.y + v.z * v.z * v.w * v.w); }
    inline float Mag2(const Vector4 MREF v)										{ return v.x * v.x + v.y * v.y + v.z * v.z * v.w * v.w; }
    
    inline Vector4 Normalize(const Vector4 MREF v)
    {
        float magnitude = Mag(v);
		float magnitudeInv = 1.0f / magnitude;
        return {v.x * magnitudeInv, v.y * magnitudeInv, v.z * magnitudeInv, v.w * magnitudeInv};
    }
	
	inline Vector3 Lerp(Vector3 MREF a, Vector3 MREF b, float alpha)
	{
		return a * (1.0f - alpha) + b * alpha;
	}
	
	inline Vector3 GetXYZ(const Vector4 MREF v) { return {v.x, v.y, v.z}; }
	inline Vector2 GetXY(const Vector4 MREF v) { return {v.x, v.y}; }
	inline Vector2 GetZW(const Vector4 MREF v) { return {v.z, v.w}; }
	
	inline Vector4 CreateVector4(const Vector3 MREF a, float w)
	{
		return {a.x, a.y, a.z, w};
	}
	
	// Catmull-Rom spline
	constexpr double CATMULL_ROM_ALPHA = 0.5;
	
	inline float CatmullRomGetT(const float t, const Vector4 MREF P0, const Vector4 MREF P1)
	{
		const float a = Pow(P1.x - P0.x, 2.0f) + Pow(P1.y - P0.y, 2.0f) + Pow(P1.z - P0.z, 2.0f) + Pow(P1.w - P0.w, 2.0f);
		const float b = Pow(a, 0.5f);
		const float c = Pow(b, CATMULL_ROM_ALPHA);
		return c + t;
	}
	
	// interpolation is done only between P1 and P2
	inline Vector4 SplineCatmullRom(const Vector4 MREF P0, const Vector4 MREF P1, const Vector4 MREF P2, const Vector4 MREF P3, const float t)
	{
		const float t0 = 0.0f;
		const float t1 = CatmullRomGetT(t0, P0, P1);
		const float t2 = CatmullRomGetT(t1, P1, P2);
		const float t3 = CatmullRomGetT(t2, P2, P3);
		
		const Vector4 A1 = (t1-t)/(t1-t0) * P0 + (t-t0)/(t1-t0) * P1;
		const Vector4 A2 = (t2-t)/(t2-t1) * P0 + (t-t1)/(t2-t1) * P1;
		const Vector4 A3 = (t3-t)/(t3-t2) * P0 + (t-t2)/(t3-t2) * P1;
		
		const Vector4 B1 = (t2-t)/(t2-t0) * A1 + (t-t0)/(t2-t0) * A2;
		const Vector4 B2 = (t3-t)/(t3-t1) * A2 + (t-t1)/(t3-t1) * A3;
		
		const Vector4 C = (t2-t)/(t2-t1) * B1 + (t-t1)/(t2-t1) * B2;
		
		return C;
	}
	
	inline float CatmullRomGetT(const float t, const float P0, const float P1)
	{
		const float a = Pow(P1 - P0, 2.0f);
		const float b = Pow(a, 0.5f);
		const float c = Pow(b, CATMULL_ROM_ALPHA);
		return c + t;
	}
	
	// interpolation is done only between P1 and P2
	inline float SplineCatmullRom(const float P0, const float P1, const float P2, const float P3, const float t)
	{
		const float t0 = 0.0f;
		const float t1 = CatmullRomGetT(t0, P0, P1);
		const float t2 = CatmullRomGetT(t1, P1, P2);
		const float t3 = CatmullRomGetT(t2, P2, P3);
		
		const float A1 = (t1-t)/(t1-t0) * P0 + (t-t0)/(t1-t0) * P1;
		const float A2 = (t2-t)/(t2-t1) * P0 + (t-t1)/(t2-t1) * P1;
		const float A3 = (t3-t)/(t3-t2) * P0 + (t-t2)/(t3-t2) * P1;
		
		const float B1 = (t2-t)/(t2-t0) * A1 + (t-t0)/(t2-t0) * A2;
		const float B2 = (t3-t)/(t3-t1) * A2 + (t-t1)/(t3-t1) * A3;
		
		const float C = (t2-t)/(t2-t1) * B1 + (t-t1)/(t2-t1) * B2;
		
		return C;
	}
	//--
}
