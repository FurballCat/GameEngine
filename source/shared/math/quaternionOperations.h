#pragma once
#include "api.h"
#include "types.h"
#include "scalarOperations.h"

namespace math
{
    inline Quaternion CreateQuaternionRotationAxis(float x, float y, float z, float angle)
    {
        const float a2 = angle/2.0f;
        const float s = sin(a2);
        const float c = cos(a2);
        return {x * s, y * s, z * s, c};
    }
	
	inline Quaternion Mul(const Quaternion MREF a, const Quaternion MREF b)
	{
		return {a.r * b.r - a.i * b.i - a.j * b.j - a.k * b.k,
				a.r * b.i - a.i * b.r - a.j * b.k - a.k * b.j,
				a.r * b.j - a.i * b.k - a.j * b.r - a.k * b.i,
				a.r * b.k - a.i * b.j - a.j * b.i - a.k * b.r};
	}
	
	inline Quaternion operator*(const Quaternion MREF a, const Quaternion MREF b)
	{
		return {a.r * b.r - a.i * b.i - a.j * b.j - a.k * b.k,
			a.r * b.i - a.i * b.r - a.j * b.k - a.k * b.j,
			a.r * b.j - a.i * b.k - a.j * b.r - a.k * b.i,
			a.r * b.k - a.i * b.j - a.j * b.i - a.k * b.r};
	}
	
	inline Quaternion& operator*=(Quaternion& a, const Quaternion MREF b)
	{
		a.r = a.r * b.r - a.i * b.i - a.j * b.j - a.k * b.k;
		a.i = a.r * b.i - a.i * b.r - a.j * b.k - a.k * b.j;
		a.j = a.r * b.j - a.i * b.k - a.j * b.r - a.k * b.i;
		a.k = a.r * b.k - a.i * b.j - a.j * b.i - a.k * b.r;
		return a;
	}
	
	inline Quaternion operator*(const Quaternion MREF a, const float b)
	{
		return {a.i * b, a.j * b, a.k * b, a.r * b};
	}
	
	inline Quaternion operator+(const Quaternion MREF a, const Quaternion MREF b)
	{
		return {a.i + b.i, a.j + b.j, a.k + b.k, a.r + b.r};
	}
	
	inline Quaternion CreateQuaternionFromEulerAnglesYZPXRY(EulerAngles angles)
	{
		Quaternion p = CreateQuaternionRotationAxis(1.0f, 0.0f, 0.0f, angles.pitch);
		Quaternion r = CreateQuaternionRotationAxis(0.0f, 1.0f, 0.0f, angles.roll);
		Quaternion y = CreateQuaternionRotationAxis(0.0f, 0.0f, 1.0f, angles.yaw);
		
		return y * p * r;
	}
	
	inline Quaternion Lerp(Quaternion MREF a, Quaternion MREF b, float alpha)
	{
		return a * (1.0f - alpha) + b * alpha;
	}
	
	inline Quaternion Slerp(const Quaternion MREF a, const Quaternion MREF b, const float alpha)
	{
		const float t = 1.0f - alpha;
		const float theta = acosf(a.i * b.i + a.j * b.j + a.k * b.k + a.r * b.r);
		const float sn = sinf(theta);
		const float w_a = sinf(t * theta) / sn;
		const float w_b = sinf(alpha * theta) / sn;
		
		Quaternion r;
		r.i = w_a * a.i + w_b * b.i;
		r.j = w_a * a.j + w_b * b.j;
		r.k = w_a * a.k + w_b * b.k;
		r.r = w_a * a.r + w_b * b.r;
		
		return r;
	}
	
	inline Quaternion Normalize(const Quaternion MREF a)
	{
		const float n = sqrt(a.i * a.i + a.j * a.j + a.k * a.k + a.r * a.r);
		return {a.i / n, a.j / n, a.k / n, a.r / n};
	}
	
	inline void NormalizeRef(Quaternion& a)
	{
		const float n = sqrt(a.i * a.i + a.j * a.j + a.k * a.k + a.r * a.r);
		a.i /= n;
		a.j /= n;
		a.k /= n;
		a.r /= n;
	}
	
	inline Quaternion ToQuaternion(const Vector4 MREF v) { return {v.x, v.y, v.z, v.w}; }
	
}
