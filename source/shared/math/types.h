#pragma once
#include "api.h"

/* Notes:
	INTRODUCTION

	Main goals for this math library is to be as fast as possible and as clear and readable as possible.
	It's code is clean but carefully designed in order to be auto vectorized by clang compiler.

	COMPILATION AND OPTIMIZATION

	To get most out of this math library, you should compile it with AVX2 on.
	For clang compiler the best results are with compiler flags: -O3 -ffast-math -mfma -mavx2
	It can be seen in dot product. If you're curious check dot product on clang with and without
	those compiler flags, as well as combinations of those flags.
	There's a special macro for references when passing arguments to functions: MREF
	this is because when using AVX2, passing by value can be optimized to passing by registers,
	which is even faster. However this does not apply to debug compilation, that's why in that
	case values are passed by reference. However, only first few parameters can be passed by
	registers.

	COORDINATE SYSTEM AND MATRICES

	z
	|   y       This math library uses right-handed coordinate system, compatible with Blender 3D.
	| /         That means X pointing to right, Y pointing forward (into the screen)
	|/____ x    and Z pointing upward.
 
	 y |
	   |___ x   However, Metal coordinates are like OpenGL coordinates, except NDC, which is 2x2x1 with origin in [0, 0, 0.5]
	   /        compared to OpenGL's 2x2x2 with origin in [0, 0, 0] (this matters in orthogonal projection).
	z /         That means that worldToCamera matrix has to rotate world -90 degrees around X axis (from Y axis to Z axis).
 
	Note that in animation - root rig bone has to also be rotated -90 degrees around X axis, as joints in Blender 3D (and most other software)
	have different coordinates than this math library. We have to only rotate root bone, the rest of the bones will just have different coordinates.

	Matrices are raw major, which means they have to be transposed when applied to shaders.
	Think of it as forward, side, up vectors being written in raws of a matrix, or to put
	it alternatively: base vectors are layed out continously in memory. That means:
	i.x, i.y, i.z, i.w, j.x, j.y, j.z, j.w, k.x ...

	Matrix multiplication is translation * rotation. That means, first operation is the last one.
	We are concatenating matrices backward: M3 * M2 * M1 (first operation is actually M1).
 
	Metal note:
	“Metal defines its Normalized Device Coordinate (NDC) system as a 2x2x1 cube with its center at (0, 0, 0.5). The left and bottom for x and y,
	respectively, of the NDC system are specified as -1. The right and top for x and y, respectively, of the NDC system are specified as +1.”
 
 */

#ifdef DEBUG
#define MREF &
#else
#define MREF
#endif

namespace math
{
	constexpr double c_e = 2.71828182845904523536028747135266250;            // e
	constexpr double c_log2e = 1.44269504088896340735992468100189214;        // log2(e)
	constexpr double c_log10e = 0.434294481903251827651128918916605082;      // log10(e)
	constexpr double c_loge2 = 0.693147180559945309417232121458176568;       // loge(2)
	constexpr double c_loge10 = 2.30258509299404568401799145468436421;       // loge(10)
	constexpr double c_pi = 3.14159265358979323846264338327950288;           // pi
	constexpr double c_pi_2 = 1.57079632679489661923132169163975144;         // pi/2
	constexpr double c_pi_4 = 0.785398163397448309615660845819875721;        // pi/4
	constexpr double c_1_pi = 0.318309886183790671537767526745028724;        // 1/pi
	constexpr double c_2_pi = 0.636619772367581343075535053490057448;        // 2/pi
	constexpr double c_2_sqrtpi = 1.12837916709551257389615890312154517;     // 2/sqrt(pi)
	constexpr double c_sqrt2 = 1.41421356237309504880168872420969808;        // sqrt(2)
	constexpr double c_1_sqrt2 = 0.707106781186547524400844362104849039;     // 1/sqrt(2)

	struct Vector2
	{
		float x, y;
	};
	
	struct Vector3
	{
		float x, y, z;
	};
	
	struct alignas( 16 ) Vector4
	{
		float x, y, z, w;
	};

	struct EulerAngles
	{
		float yaw, pitch, roll;
	};
	
	struct alignas( 16 ) Quaternion
	{
		float i, j, k, r;
	};

	struct alignas( 16 ) Matrix4x4        // in theory should be aligned to 32 to use ymm registers, but practically it doesn't affect assembly code
	{
		Vector4 x, y, z, w;
	};

	struct alignas( 16 ) Transform
	{
		Vector4 translation;
		Quaternion rotation;
	};
    
    struct UintVector3
    {
        unsigned int x, y, z;
    };
	
	struct UintVector4
	{
		unsigned int x, y, z, w;
	};
	
	struct IntVector4
	{
		int x, y, z, w;
	};
	
	struct BBox
	{
		Vector4 m_min;
		Vector4 m_max;
	};
}

MATH_API void TestMathExport();























