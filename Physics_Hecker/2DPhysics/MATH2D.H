/*----------------------------------------------------------------------------

2D Physics Test Program - a cheesy test harness for 2D physics

by Chris Hecker for my Game Developer Magazine articles.  See my homepage
for more information.

NOTE: This is a hacked test program, not a nice example of Windows programming.
physics.cpp the only part of this you should look at!!!

This material is Copyright 1997 Chris Hecker, All Rights Reserved.
It's for you to read and learn from, not to put in your own articles
or books or on your website, etc.  Thank you.

Chris Hecker
checker@d6.com
http://www.d6.com/users/checker

*/

/*----------------------------------------------------------------------------

math2d.h - The simple math library for the 2D physics demo.

10/13/96 - checker

*/

#if !defined(MATH2D_H)
#define MATH2D_H

// explicit dependencies
#include <math.h>		// for sqrt
#include <float.h>

/*----------------------------------------------------------------------------

Type and constant declarations.

*/

typedef float real;
typedef real r;			// for constants, ie. r(1), not for declaration use

real const REAL_MAX = FLT_MAX;
real const REAL_MIN = FLT_MIN;

real const PI = r(3.14159265358979323846);
real const Epsilon = r(0.00001);

struct vector_2
{
	real X, Y;

	inline vector_2( void );
	inline vector_2( real X, real Y );

	inline vector_2 &operator+=( vector_2 const &A );
	inline vector_2 &operator-=( vector_2 const &A );
};

struct matrix_2x2
{
	real aElements[2][2];

	inline matrix_2x2( void );
	inline matrix_2x2( real RotateByThisManyRadians );
};



/*----------------------------------------------------------------------------

Functions.

*/

inline real RadiansFrom( real Degrees );
inline real DegreesFrom( real Radians );

inline vector_2 operator-( vector_2 const &A, vector_2 const &B );
inline vector_2 operator+( vector_2 const &A, vector_2 const &B );
inline vector_2 operator*( real A, vector_2 const &B );
inline vector_2 operator*( vector_2 const &A, real B );
inline vector_2 operator/( vector_2 const &A, real B );

inline real DotProduct( vector_2 const &A, vector_2 const &B );

// A-perp dot B
inline real PerpDotProduct( vector_2 const &A, vector_2 const &B );

inline vector_2 GetPerpendicular( vector_2 const &A );
inline real GetLength( vector_2 const &A );
inline vector_2 GetNormal( vector_2 const &A );	// @todo need this?

inline vector_2 operator*( matrix_2x2 const &A, vector_2 const &B );

/*----------------------------------------------------------------------------

Inline function definitions.

*/

inline real RadiansFrom( real Degrees )
{
	return (Degrees * PI) / r(180);
}

inline real DegreesFrom( real Radians )
{
	return (Radians * r(180)) / PI;
}

inline vector_2 operator-( vector_2 const &A, vector_2 const &B )
{
	return vector_2(A.X-B.X,A.Y-B.Y);
}

inline vector_2 operator+( vector_2 const &A, vector_2 const &B )
{
	return vector_2(A.X+B.X,A.Y+B.Y);
}

inline vector_2 operator*( real A, vector_2 const &B )
{
	return vector_2(A*B.X,A*B.Y);
}

inline vector_2 operator*( vector_2 const &A, real B )
{
	return vector_2(B*A.X,B*A.Y);
}

inline vector_2 operator/( vector_2 const &A, real B )
{
	return vector_2(A.X/B,A.Y/B);
}

inline real DotProduct( vector_2 const &A, vector_2 const &B )
{
	return A.X*B.X + A.Y*B.Y;
}

inline real PerpDotProduct( vector_2 const &A, vector_2 const &B )
{
	return A.X*B.Y - A.Y*B.X;
}

inline vector_2 GetPerpendicular( vector_2 const &A )
{
	return vector_2(-A.Y,A.X);
}

inline real GetLength( vector_2 const &A )
{
	return r(sqrt(A.X*A.X + A.Y*A.Y));
}

inline vector_2 GetNormal( vector_2 const &A )
{
	real OneOverLength = r(1)/GetLength(A);
	return vector_2(OneOverLength*A.X,OneOverLength*A.Y);
}

inline vector_2::vector_2( void ) : X(r(0)), Y(r(0))
{
}

inline vector_2::vector_2( real X_, real Y_ ) : X(X_), Y(Y_)
{
}

inline vector_2 &vector_2::operator+=( vector_2 const &A )
{
	X += A.X;
	Y += A.Y;
	return *this;
}
inline vector_2 &vector_2::operator-=( vector_2 const &A )
{
	X -= A.X;
	Y -= A.Y;
	return *this;
}

inline vector_2 operator*( matrix_2x2 const &A, vector_2 const &B )
{
	return vector_2(A.aElements[0][0]*B.X + A.aElements[0][1]*B.Y,
					A.aElements[1][0]*B.X + A.aElements[1][1]*B.Y);
}

inline matrix_2x2::matrix_2x2( void )
{
	aElements[0][0] = aElements[0][1] = aElements[1][0] = aElements[1][1] =r(0);
}

inline matrix_2x2::matrix_2x2( real Radians )
{
	real const CosAngle = (real)cos(Radians);
	real const SinAngle = (real)sin(Radians);

	aElements[0][0] = CosAngle; aElements[0][1] = -SinAngle;
	aElements[1][0] = SinAngle; aElements[1][1] =  CosAngle;
}

#endif



