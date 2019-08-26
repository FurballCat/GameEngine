#pragma once
#include "api.h"
#include "types.h"
#include "scalarOperations.h"
#include "vectorOperations.h"

namespace math
{
    constexpr Matrix4x4 CreateMatrix4x4Identity()
    {
        return {{1, 0, 0, 0},
                {0, 1, 0, 0},
                {0, 0, 1, 0},
                {0, 0, 0, 1}};
    }
    
    constexpr Matrix4x4 CreateMatrix4x4Ones()
    {
        return {{1, 1, 1, 1},
            {1, 1, 1, 1},
            {1, 1, 1, 1},
            {1, 1, 1, 1}};
    }
    
    constexpr Matrix4x4 CreateMatrix4x4Translation(const Vector4 u)
    {
        return {{1, 0, 0, 0},
                {0, 1, 0, 0},
                {0, 0, 1, 0},
                {u.x, u.y, u.z, 1}};
    }
    
    constexpr Matrix4x4 CreateMatrix4x4Translation(const float x, const float y, const float z)
    {
        return {{1, 0, 0, 0},
                {0, 1, 0, 0},
                {0, 0, 1, 0},
                {x, y, z, 1}};
    }
	
	inline Matrix4x4 CreateMatrix4x4LookAt(const Vector3 eye, const Vector3 at, const Vector3 up)
	{
		Vector3 axisY = Normalize(at - eye);
		Vector3 axisX = Normalize(Cross(axisY, up));
		Vector3 axisZ = Cross(axisX, axisY);
		
		return {{axisX.x, axisX.y, axisX.z, -Dot(axisX, eye)},
			{axisY.x, axisY.y, axisY.z, -Dot(axisY, eye)},
			{axisZ.x, axisZ.y, axisZ.z, -Dot(axisZ, eye)},
			{0, 0, 0, 1}};
	}
	
	constexpr Matrix4x4 CreateMatrix4x4OrthographicProjection(const float b, const float t, const float l, const float r, const float n, const float f)
	{
		return {{2 / (r - l), 0, 0, -(r + l) / (r - l)},
				{0, 2 / (t - b), 0, -(t + b) / (t - b)},
				{0, 0, -1 / (f - n), -n / (f - n)},
				{0, 0, 0, 1}};
	}
	
	// “Metal defines its Normalized Device Coordinate (NDC) system as a 2x2x1 cube with its center at (0, 0, 0.5). The left and bottom for x and y,
	// respectively, of the NDC system are specified as -1. The right and top for x and y, respectively, of the NDC system are specified as +1.”
	inline Matrix4x4 CreateMatrix4x4OrthographicProjection(const float width, const float height, const float near, const float far)
	{
		const float halfWidth = width / 2.0f;
		const float halfHeight = height / 2.0f;
		
		const float left = -halfWidth;
		const float bottom = -halfHeight;
		const float right = halfWidth;
		const float top = halfHeight;
		
		return CreateMatrix4x4OrthographicProjection(bottom, top, left, right, near, far);
	}
	
    // b - bottom, t - top, l - left, r - right, n - near, f - far
    constexpr Matrix4x4 CreateMatrix4x4Projection(const float b, const float t, const float l, const float r, const float n, const float f)
    {
        return {{2 * n / (r - l), 0, (r + l) / (r - l), 0},
                {0, 2 * n / (t - b), (t + b) / (t - b), 0},
                {0, 0, -(f + n) / (f - n), -2 * f * n / (f - n)},
                {0, 0, -1, 0}};
    }
    
    inline Matrix4x4 CreateMatrix4x4Projection(const float fov, const float aspectRatio, const float near, const float far)
    {
        float scale = Tan(fov * 0.5f * c_pi / 180.0f) * near;
        float top = scale;
        float bottom = -top;
        float right = aspectRatio * scale;
        float left = -right;
        
        return CreateMatrix4x4Projection(bottom, top, left, right, near, far);
    }
    
    inline Matrix4x4 CreateMatrix4x4RotationZ(const float phi)
    {
        return {{Cos(phi), -Sin(phi), 0, 0},
                {Sin(phi), Cos(phi), 0, 0},
                {0, 0, 1, 0},
                {0, 0, 0, 1}};
    }
    
    inline Matrix4x4 CreateMatrix4x4RotationY(const float phi)
    {
        return {{Cos(phi), 0, Sin(phi), 0},
                {0, 0, 1, 0},
                {-Sin(phi), 0, Cos(phi), 0},
                {0, 0, 0, 1}};
    }
    
    inline Matrix4x4 CreateMatrix4x4RotationX(const float phi)
    {
        return {{1, 0, 0, 0},
                {0, Cos(phi), -Sin(phi), 0},
                {0, Sin(phi), Cos(phi), 0},
                {0, 0, 0, 1}};
    }
    
    inline Matrix4x4 Transpose(const Matrix4x4& M)
    {
        return {{M.x.x, M.y.x, M.z.x, M.w.x},
                {M.x.y, M.y.y, M.z.y, M.w.y},
                {M.x.z, M.y.z, M.z.z, M.w.z},
                {M.x.w, M.y.w, M.z.w, M.w.w}};
    }
    
    inline Matrix4x4 Mul(const Matrix4x4& A, const Matrix4x4& B)
    {
        return {{A.x.x * B.x.x + A.x.y * B.y.x + A.x.z * B.z.x + A.x.w * B.w.x,
                 A.x.x * B.x.y + A.x.y * B.y.y + A.x.z * B.z.y + A.x.w * B.w.y,
                 A.x.x * B.x.z + A.x.y * B.y.z + A.x.z * B.z.z + A.x.w * B.w.z,
                 A.x.x * B.x.w + A.x.y * B.y.w + A.x.z * B.z.w + A.x.w * B.w.w},
                {A.y.x * B.x.x + A.y.y * B.y.x + A.y.z * B.z.x + A.y.w * B.w.x,
                 A.y.x * B.x.y + A.y.y * B.y.y + A.y.z * B.z.y + A.y.w * B.w.y,
                 A.y.x * B.x.z + A.y.y * B.y.z + A.y.z * B.z.z + A.y.w * B.w.z,
                 A.y.x * B.x.w + A.y.y * B.y.w + A.y.z * B.z.w + A.y.w * B.w.w},
                {A.z.x * B.x.x + A.z.y * B.y.x + A.z.z * B.z.x + A.z.w * B.w.x,
                 A.z.x * B.x.y + A.z.y * B.y.y + A.z.z * B.z.y + A.z.w * B.w.y,
                 A.z.x * B.x.z + A.z.y * B.y.z + A.z.z * B.z.z + A.z.w * B.w.z,
                 A.z.x * B.x.w + A.z.y * B.y.w + A.z.z * B.z.w + A.z.w * B.w.w},
                {A.w.x * B.x.x + A.w.y * B.y.x + A.w.z * B.z.x + A.w.w * B.w.x,
                 A.w.x * B.x.y + A.w.y * B.y.y + A.w.z * B.z.y + A.w.w * B.w.y,
                 A.w.x * B.x.z + A.w.y * B.y.z + A.w.z * B.z.z + A.w.w * B.w.z,
                 A.w.x * B.x.w + A.w.y * B.y.w + A.w.z * B.z.w + A.w.w * B.w.w}};
    }
	
	inline Matrix4x4 operator*(const Matrix4x4& A, const Matrix4x4& B)
	{
		return Mul(A, B);
	}
    
    inline Vector4 Mul(const Vector4 v, const Matrix4x4& M)
    {
        return {v.x * M.x.x + v.y * M.y.x + v.z * M.z.x + v.w * M.w.x,
                v.x * M.x.y + v.y * M.y.y + v.z * M.z.y + v.w * M.w.y,
                v.x * M.x.z + v.y * M.y.z + v.z * M.z.z + v.w * M.w.z,
                v.x * M.x.w + v.y * M.y.w + v.z * M.z.w + v.w * M.w.w};
    }
    
    inline Vector4 Project(const Vector4 v, const Matrix4x4& M)
    {
        Vector4 r = {v.x * M.x.x + v.y * M.y.x + v.z * M.z.x + v.w * M.w.x,
            v.x * M.x.y + v.y * M.y.y + v.z * M.z.y + v.w * M.w.y,
            v.x * M.x.z + v.y * M.y.z + v.z * M.z.z + v.w * M.w.z,
            v.x * M.x.w + v.y * M.y.w + v.z * M.z.w + v.w * M.w.w};
        
        if(r.w != 1.0f)
        {
            r.x /= r.w;
            r.y /= r.w;
            r.z /= r.w;
            r.w = 1.0f;
        }
        
        return r;
    }
	
	inline void ConvertTransformToMatrix(const Transform& t, Matrix4x4& M)
	{
		const Quaternion& q = t.rotation;
		M.x.x = 1.0f - 2.0f * q.j * q.j - 2.0f * q.k * q.k;
		M.x.y = 2.0f * q.i * q.j + 2.0f * q.k * q.r;
		M.x.z = 2.0f * q.i * q.k - 2.0f * q.j * q.r;
		M.x.w = 0.0f;
		
		M.y.x = 2.0f * q.i * q.j - 2.0f * q.k * q.r;
		M.y.y = 1.0f - 2.0f * q.i * q.i - 2.0f * q.k * q.k;
		M.y.z = 2.0f * q.j * q.k + 2.0f * q.i * q.r;
		M.y.w = 0.0f;
		
		M.z.x = 2.0f * q.i * q.k + 2.0f * q.j * q.r;
		M.z.y = 2.0f * q.j * q.k - 2.0f * q.i * q.r;
		M.z.z = 1.0f - 2.0f * q.i * q.i - 2.0f * q.j * q.j;
		M.z.w = 0.0f;
		
		M.w = t.translation;
		M.w.w = 1.0f;
	}
}
