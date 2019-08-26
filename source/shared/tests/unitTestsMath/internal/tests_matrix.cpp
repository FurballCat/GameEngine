#include "pch.h"

#include "math/types.h"
#include "math/matrixOperations.h"
#include "math/vectorOperations.h"

#include "unitTests/unitTestsFramework.h"

using namespace test;
using namespace math;

UNITTEST(Math, MatrixMultiplicationIdentity)
{
    Matrix4x4 I = CreateMatrix4x4Identity();
    Matrix4x4 R = Mul(I, I);
    
    Assert::AreEqual(R.x.x, I.x.x);
    Assert::AreEqual(R.x.y, I.x.y);
    Assert::AreEqual(R.x.z, I.x.z);
    Assert::AreEqual(R.x.w, I.x.w);
    
    Assert::AreEqual(R.y.x, I.y.x);
    Assert::AreEqual(R.y.y, I.y.y);
    Assert::AreEqual(R.y.z, I.y.z);
    Assert::AreEqual(R.y.w, I.y.w);
    
    Assert::AreEqual(R.z.x, I.z.x);
    Assert::AreEqual(R.z.y, I.z.y);
    Assert::AreEqual(R.z.z, I.z.z);
    Assert::AreEqual(R.z.w, I.z.w);
    
    Assert::AreEqual(R.w.x, I.w.x);
    Assert::AreEqual(R.w.y, I.w.y);
    Assert::AreEqual(R.w.z, I.w.z);
    Assert::AreEqual(R.w.w, I.w.w);
}

UNITTEST(Math, MatrixMultiplicationOnes)
{
    Matrix4x4 I = CreateMatrix4x4Ones();
    Matrix4x4 R = Mul(I, I);
    
    Assert::AreEqual(R.x.x, 4);
    Assert::AreEqual(R.x.y, 4);
    Assert::AreEqual(R.x.z, 4);
    Assert::AreEqual(R.x.w, 4);
    
    Assert::AreEqual(R.y.x, 4);
    Assert::AreEqual(R.y.y, 4);
    Assert::AreEqual(R.y.z, 4);
    Assert::AreEqual(R.y.w, 4);
    
    Assert::AreEqual(R.z.x, 4);
    Assert::AreEqual(R.z.y, 4);
    Assert::AreEqual(R.z.z, 4);
    Assert::AreEqual(R.z.w, 4);
    
    Assert::AreEqual(R.w.x, 4);
    Assert::AreEqual(R.w.y, 4);
    Assert::AreEqual(R.w.z, 4);
    Assert::AreEqual(R.w.w, 4);
}

UNITTEST(Math, VectorTransformIdentity)
{
    Vector4 v = CreateVectorOnes();
    Matrix4x4 M = CreateMatrix4x4Identity();
    Vector4 r = Mul(v, M);
    
    Assert::AreEqual(v.x, r.x);
    Assert::AreEqual(v.y, r.y);
    Assert::AreEqual(v.z, r.z);
    Assert::AreEqual(v.w, r.w);
}

UNITTEST(Math, PointTranslation)
{
    Vector4 v = CreateVectorOnes();
    Matrix4x4 M = CreateMatrix4x4Translation(1.0f, 2.0f, 3.0f);
    Vector4 r = Mul(v, M);
    
    Assert::AreEqual(r.x, 2.0f);
    Assert::AreEqual(r.y, 3.0f);
    Assert::AreEqual(r.z, 4.0f);
    Assert::AreEqual(r.w, 1.0f);
}

UNITTEST(Math, VectorTranslation)
{
    Vector4 v = CreateVectorOnes();
    v.w = 0.0f; // change point to vector
    Matrix4x4 M = CreateMatrix4x4Translation(1.0f, 2.0f, 3.0f);
    Vector4 r = Mul(v, M);
    
    Assert::AreEqual(r.x, 1.0f);
    Assert::AreEqual(r.y, 1.0f);
    Assert::AreEqual(r.z, 1.0f);
    Assert::AreEqual(r.w, 0.0f);
}

UNITTEST(Math, Projection)
{
    Vector4 v = {2, 1, -20, 1};      // 10 meters forward relative to camera
    Matrix4x4 M = CreateMatrix4x4Projection(120, 1, 1, 100);
    Vector4 r = Project(v, M);
    
    //Assert::AreEqual(r.x, 1.0f);
    //Assert::AreEqual(r.y, 1.0f);
    //Assert::AreEqual(r.z, 1.0f);
    //Assert::AreEqual(r.w, 0.0f);
    // todo: fix it
}

UNITTEST(Math, VectorTransformation)
{
    Vector4 i = {1, 1, 0, 0};
    Vector4 j = {0, 1, 1, 0};
    Vector4 k = {0, 0, 1, 0};
    Matrix4x4 M = { i, j, k, {0, 0, 0, 1} };
    
    Vector4 arr[4] = {{1, 0, 0, 0}, {1, 2, 3, 0}, {0, 2, 1, 0}, {4, 3, 2, 0}};
    for(auto a : arr)
    {
        Vector4 a_t = Mul(a, M);
        Vector4 a_p = i * a.x + j * a.y + k * a.z;
        Assert::AreEqual(a_t.x, a_p.x);
        Assert::AreEqual(a_t.y, a_p.y);
        Assert::AreEqual(a_t.z, a_p.z);
        Assert::AreEqual(a_t.w, a_p.w);
    }
}

UNITTEST(Math, VectorRotationZ)
{
    Vector4 v = {1, 0, 0, 0};
    float angleInRadians = c_pi_2;
    Matrix4x4 M = CreateMatrix4x4RotationZ(angleInRadians);
    
    const float eps = 0.0000001;
    
    Vector4 v_p = Mul(v, M);
    Assert::AreEqual(v_p.x, 0, eps);
    Assert::AreEqual(v_p.y, 1, eps);
    Assert::AreEqual(v_p.z, 0, eps);
    Assert::AreEqual(v_p.w, 0, eps);
}

UNITTEST(Math, VectorRotationY)
{
    Vector4 v = {0, 0, 1, 0};
    float angleInRadians = c_pi_2;
    Matrix4x4 M = CreateMatrix4x4RotationY(angleInRadians);
    
    const float eps = 0.0000001;
    
    Vector4 v_p = Mul(v, M);
    Assert::AreEqual(v_p.x, 1, eps);
    Assert::AreEqual(v_p.y, 0, eps);
    Assert::AreEqual(v_p.z, 0, eps);
    Assert::AreEqual(v_p.w, 0, eps);
}

UNITTEST(Math, VectorRotationX)
{
    Vector4 v = {0, 1, 0, 0};
    float angleInRadians = c_pi_2;
    Matrix4x4 M = CreateMatrix4x4RotationX(angleInRadians);
    
    const float eps = 0.0000001;
    
    Vector4 v_p = Mul(v, M);
    Assert::AreEqual(v_p.x, 0, eps);
    Assert::AreEqual(v_p.y, 0, eps);
    Assert::AreEqual(v_p.z, 1, eps);
    Assert::AreEqual(v_p.w, 0, eps);
}
