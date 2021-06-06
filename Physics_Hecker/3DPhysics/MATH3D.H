/*----------------------------------------------------------------------------

3D Physics Test Program - a cheesy test harness for 3D physics

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

#if !defined(_MATH3D_H_)

// explicit dependencies
#include <math.h>       // for sqrt
#include <assert.h>

/* Remove Macintosh lameness */
#if defined(Length)
#undef Length
#endif

#pragma warning (disable:4244)      // int to float conversion

/*----------------------------------------------------------------------------

This header contains the declarations for the dumb3d functions.

*/

// forward declarations

#include <float.h>      // for min/max
#include <assert.h>

// real type
typedef float real;
typedef real r;         // for constants, ie. r(1), not for declaration use

real const REAL_MAX = FLT_MAX;
real const REAL_MIN = FLT_MIN;

real const PI = r(3.14159265358979323846);
real const Epsilon = r(0.00001);

class vector_3;
class matrix_3x3;

/*----------------------------------------------------------------------------

globally useful functions.

*/

inline void CreateOpenGLTransform( matrix_3x3 const &Orientation,
                vector_3 const &Position, float *pM);

inline vector_3 operator+( vector_3 const &Operand1,
        vector_3 const &Operand2 );

inline vector_3 operator-( vector_3 const &Operand1,
        vector_3 const &Operand2 );

inline vector_3 operator*( vector_3 const &Multiplicand,
        real const &Multiplier );

inline vector_3 operator*( real const &Multiplier,
        vector_3 const &Multiplicand );
      
inline vector_3 operator/( vector_3 const &Quotient,
        real const &Divisor );

inline vector_3 operator-( vector_3 const &Operand1 );

inline vector_3 CrossProduct( vector_3 const &Operand1,
        vector_3 const &Operand2 );

inline vector_3 Normalize( vector_3 const &A );

void OrthonormalizeOrientation( matrix_3x3 &Orientation );

inline real DotProduct( vector_3 const &Operand1, vector_3 const &Operand2 );

inline matrix_3x3 Transpose( matrix_3x3 const &Matrix );

matrix_3x3 operator*( matrix_3x3 const &Multiplicand,
        matrix_3x3 const &Multiplier );

vector_3 operator*( matrix_3x3 const &Multiplicand,
        vector_3 const &Multiplier );

inline matrix_3x3 operator-( matrix_3x3 const &Matrix );
inline matrix_3x3 operator+( matrix_3x3 const &Operand1,
        matrix_3x3 const &Operand2 );
inline matrix_3x3 operator-( matrix_3x3 const &Operand1,
        matrix_3x3 const &Operand2 );
inline matrix_3x3 operator*( real const Value, matrix_3x3 const &Matrix );
inline matrix_3x3 operator*( matrix_3x3 const &Matrix, real const Value );

/*----------------------------------------------------------------------------

triple. Base class for vectors and points.

*/

class triple
{
public:

    inline real &operator()( int unsigned Row );
    inline real const &operator()( int unsigned Row ) const;

    inline real GetElement( int Row ) const;

    inline real GetX( void ) const;
    inline real GetY( void ) const;
    inline real GetZ( void ) const;

    inline void SetElement( int Row, real Value );

    inline void SetX( real Value );
    inline void SetY( real Value );
    inline void SetZ( real Value );

protected:

    inline triple( void );
    inline triple( real X, real Y, real Z );
    inline triple( triple const & );
    inline triple &operator=( triple const & );

    real aElements[3];
};


/*----------------------------------------------------------------------------

vector_3.  This class represents a 3D vector.

*/

class vector_3 :
    public triple
{
public:

    inline vector_3( void );
    inline vector_3( real const *pElements );
    inline vector_3( real X, real Y, real Z = 0);

    inline vector_3 &operator+=(vector_3 const &addend);
    inline vector_3 &operator-=(vector_3 const &lessor);
    inline vector_3 &operator*=(real const multiplier);
    inline vector_3 &operator/=(real const divisor);

    vector_3 &Normalize( void );
    real Length( void ) const;

    real const *GetElements( void ) const { return aElements; };
};



/*----------------------------------------------------------------------------

matrix_3x3.  This class represents row major 3x3 homogeneous matrices.

*/

class matrix_3x3
{
public:

    matrix_3x3( void );
    matrix_3x3( real (*pElements)[3] );

    // @todo this isn't the safest ctor to have
    enum mode { SkewSymmetric, Zero };
    inline matrix_3x3( mode MustBeZero );
    inline matrix_3x3( vector_3 const &CrossVector, mode MustBeSkewSymmetric );
    
    inline matrix_3x3 &operator+=( matrix_3x3 const &A );

    inline real &operator()( int unsigned Row, int unsigned Column );
    inline real const &operator()( int unsigned Row, int unsigned Column ) const;

    inline real GetElement( int Row, int Column ) const;
    inline matrix_3x3 &SetElement( int Row, int Column, real Value );

protected:

    enum do_not_initialize { DoNotInitialize };

    inline matrix_3x3( do_not_initialize );

    real aElements[3][3];
};

/*----------------------------------------------------------------------------

inline function definitions.

*/

inline vector_3 &vector_3::operator+=(vector_3 const &addend)
{
    SetX(GetX() + addend.GetX());
    SetY(GetY() + addend.GetY());
    SetZ(GetZ() + addend.GetZ());

    return(*this);
}

inline vector_3 operator+( vector_3 const &Operand1,
        vector_3 const &Operand2 )
{
    return vector_3(Operand1.GetX() + Operand2.GetX(),
                    Operand1.GetY() + Operand2.GetY(),
                    Operand1.GetZ() + Operand2.GetZ());
}

inline vector_3 &vector_3::operator-=(vector_3 const &lessor)
{
    SetX(GetX() - lessor.GetX());
    SetY(GetY() - lessor.GetY());
    SetZ(GetZ() - lessor.GetZ());

    return(*this);
}

inline vector_3 operator-( vector_3 const &Operand1,
        vector_3 const &Operand2 )
{
    return vector_3(Operand1.GetX() - Operand2.GetX(),
                    Operand1.GetY() - Operand2.GetY(),
                    Operand1.GetZ() - Operand2.GetZ());
}

inline vector_3 operator-( vector_3 const &Operand1 )
{
    return vector_3(-Operand1.GetX(),-Operand1.GetY(),-Operand1.GetZ());
}

inline vector_3 &vector_3::operator*=(real const multiplier)
{
    SetX(GetX() * multiplier);
    SetY(GetY() * multiplier);
    SetZ(GetZ() * multiplier);

    return(*this);
}

inline vector_3 operator*( vector_3 const &Multiplicand,
        real const &Multiplier )
{
    return vector_3(Multiplicand.GetX() * Multiplier,
                    Multiplicand.GetY() * Multiplier,
                    Multiplicand.GetZ() * Multiplier);
}

inline vector_3 operator*( real const &Multiplier,
        vector_3 const &Multiplicand ) 
{
    return Multiplicand * Multiplier;
}
      
inline vector_3 &vector_3::operator/=(real const divisor)
{
    SetX(GetX() / divisor);
    SetY(GetY() / divisor);
    SetZ(GetZ() / divisor);

    return(*this);
}

inline vector_3 operator/( vector_3 const &Quotient,
        real const &Divisor )
{
    return Quotient * (real(1.0)/Divisor);
}

inline vector_3 CrossProduct( vector_3 const &Operand1,
        vector_3 const &Operand2 )
{
    real X = Operand1.GetY() * Operand2.GetZ() -
                Operand1.GetZ() * Operand2.GetY();
    real Y = Operand1.GetZ() * Operand2.GetX() -
                Operand1.GetX() * Operand2.GetZ();
    real Z = Operand1.GetX() * Operand2.GetY() -
                Operand1.GetY() * Operand2.GetX();

    return vector_3(X,Y,Z);
}

inline real DotProduct( vector_3 const &Operand1, vector_3 const &Operand2 )
{
    return Operand1.GetX() * Operand2.GetX() +
            Operand1.GetY() * Operand2.GetY() +
            Operand1.GetZ() * Operand2.GetZ();
}

inline real triple::GetElement( int Row ) const
{
    return aElements[Row];
}

inline real triple::GetX( void ) const
{
    return aElements[0];
}

inline real triple::GetY( void ) const
{
    return aElements[1];
}

inline real triple::GetZ( void ) const
{
    return aElements[2];
}

inline real &triple::operator()( int unsigned Row )
{
    assert(Row < 3);
    return aElements[Row];
}

inline real const &triple::operator()( int unsigned Row ) const
{
    assert(Row < 3);
    return aElements[Row];
}

inline void triple::SetElement( int Row, real Value )
{
    aElements[Row] = Value;
}

inline void triple::SetX( real Value )
{
    aElements[0] = Value;
}

inline void triple::SetY( real Value )
{
    aElements[1] = Value;
}

inline void triple::SetZ( real Value )
{
    aElements[2] = Value;
}

inline triple::triple( void )
{
    aElements[0] = aElements[1] = aElements[2] = 0;
}

inline triple::triple( real X, real Y, real Z )
{
    aElements[0] = X;
    aElements[1] = Y;
    aElements[2] = Z;
}

inline triple::triple( triple const &Source )
{
    aElements[0] = Source.aElements[0];
    aElements[1] = Source.aElements[1];
    aElements[2] = Source.aElements[2];
}

inline triple &triple::operator=( triple const &Source )
{
    aElements[0] = Source.aElements[0];
    aElements[1] = Source.aElements[1];
    aElements[2] = Source.aElements[2];

    return *this;
}

inline vector_3::vector_3( void ) :
    triple(0,0,0)
{
}

inline vector_3::vector_3( real X, real Y, real Z ) :
    triple(X,Y,Z)
{
}

inline vector_3::vector_3( real const *pElements )
{
    assert(pElements);
    aElements[0] = pElements[0];
    aElements[1] = pElements[1];
    aElements[2] = pElements[2];
}

inline real &matrix_3x3::operator()( int unsigned Row, int unsigned Column )
{
    assert((Row < 3) && (Column < 3));
    return aElements[Row][Column];
}

inline real const &matrix_3x3::operator()( int unsigned Row,
        int unsigned Column ) const
{
    assert((Row < 3) && (Column < 3));
    return aElements[Row][Column];
}

inline real matrix_3x3::GetElement( int Row, int Column ) const
{
    return aElements[Row][Column];
}

inline matrix_3x3 &matrix_3x3::SetElement( int Row, int Column, real Value )
{
    aElements[Row][Column] = Value;

    return *this;
}

inline matrix_3x3::matrix_3x3( do_not_initialize )
{
}

inline matrix_3x3::matrix_3x3( real (*pElements)[3] )
{
    assert(pElements);

    matrix_3x3 &M = *this;
    for(int i = 0;i < 3;i++)
    {
        for(int j = 0;j < 3;j++)
        {
            M(i,j) = pElements[i][j];
        }
    }
}
    
inline matrix_3x3::matrix_3x3( vector_3 const &Vector, mode Mode )
{
    assert(Mode == SkewSymmetric);

    matrix_3x3 &M = *this;

    M(0,0) =  r(0);          M(0,1) = -Vector.GetZ(); M(0,2) =  Vector.GetY();
    M(1,0) =  Vector.GetZ(); M(1,1) =  r(0);          M(1,2) = -Vector.GetX();
    M(2,0) = -Vector.GetY(); M(2,1) =  Vector.GetX(); M(2,2) = r(0);
}

inline matrix_3x3::matrix_3x3( mode Mode )
{
    assert(Mode == Zero);

    for(int Counter = 0;Counter < 3;Counter++)
    {
        aElements[Counter][0] = r(0);
        aElements[Counter][1] = r(0);
        aElements[Counter][2] = r(0);
    }
}

inline matrix_3x3 Transpose( matrix_3x3 const &Matrix )
{
    matrix_3x3 Return;

    for(int Counter = 0;Counter < 3;Counter++)
    {
        Return(0,Counter) = Matrix(Counter,0);
        Return(1,Counter) = Matrix(Counter,1);
        Return(2,Counter) = Matrix(Counter,2);
    }

    return Return;
}   

inline matrix_3x3 operator+( matrix_3x3 const &Operand1,
        matrix_3x3 const &Operand2 )
{
    matrix_3x3 Return;

    for(int Counter = 0;Counter < 3;Counter++)
    {
        Return(0,Counter) = Operand1(0,Counter) + Operand2(0,Counter);
        Return(1,Counter) = Operand1(1,Counter) + Operand2(1,Counter);
        Return(2,Counter) = Operand1(2,Counter) + Operand2(2,Counter);
    }

    return Return;
}   
    
inline matrix_3x3 operator-( matrix_3x3 const &Operand1,
        matrix_3x3 const &Operand2 )
{
    matrix_3x3 Return;

    for(int Counter = 0;Counter < 3;Counter++)
    {
        Return(0,Counter) = Operand1(0,Counter) - Operand2(0,Counter);
        Return(1,Counter) = Operand1(1,Counter) - Operand2(1,Counter);
        Return(2,Counter) = Operand1(2,Counter) - Operand2(2,Counter);
    }

    return Return;
}   
    
inline matrix_3x3 operator*( real const Value, matrix_3x3 const &Matrix )
{
    matrix_3x3 Return;

    for(int Counter = 0;Counter < 3;Counter++)
    {
        Return(0,Counter) = Value * Matrix(0,Counter);
        Return(1,Counter) = Value * Matrix(1,Counter);
        Return(2,Counter) = Value * Matrix(2,Counter);
    }

    return Return;
}   

inline matrix_3x3 &matrix_3x3::operator+=( matrix_3x3 const &Operand1 )
{
    for(int Counter = 0;Counter < 3;Counter++)
    {
        // @todo this is probably the wrong order, but I'm lazy
        aElements[0][Counter] += Operand1(0,Counter);
        aElements[1][Counter] += Operand1(1,Counter);
        aElements[2][Counter] += Operand1(2,Counter);
    }

    return *this;
}   

inline matrix_3x3 operator*( matrix_3x3 const &Matrix, real const Value )
{
    return Value * Matrix;
}

inline matrix_3x3 operator-( matrix_3x3 const &Matrix )
{
    matrix_3x3 Return;

    for(int Counter = 0;Counter < 3;Counter++)
    {
        Return(0,Counter) = -Matrix(0,Counter);
        Return(1,Counter) = -Matrix(1,Counter);
        Return(2,Counter) = -Matrix(2,Counter);
    }

    return Return;
}   

inline vector_3 Normalize( vector_3 const &A )
{
    vector_3 Return = A;
    return Return.Normalize();
}

inline void CreateOpenGLTransform( matrix_3x3 const &Orientation,
                vector_3 const &Position, float *pM)
{
    assert(pM);
    float (*pM44)[4] = (float(*)[4])pM;
    int i;
    
    // remember, OpenGL matrices are transposed!

    for(i = 0;i < 3;i++)
    {
        for(int j = 0;j < 3;j++)
        {
            pM44[j][i] = Orientation(i,j);
        }
    }

    pM44[0][3] = pM44[1][3] = pM44[2][3] = 0.0f; pM44[3][3] = 1.0f;

    for(i = 0;i < 3;i++)
    {
        pM44[3][i] = Position(i);
    }
}

#pragma warning (disable:4514)      // unreferenced inline removed
#pragma warning (default:4244)      // int to float conversion

#define _MATH3D_H_
#endif
