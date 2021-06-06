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

#include "math3d.h"

//*** We're intentionally converting doubles to floats all over...
#pragma warning ( disable : 4136 )
#pragma warning ( disable : 4244 )

#include <assert.h>
#include <math.h>

/*----------------------------------------------------------------------------

matrix multiplication.

*/

matrix_3x3 operator*( matrix_3x3 const &Multiplicand,
        matrix_3x3 const &Multiplier )
{
    matrix_3x3 ReturnMatrix;

    for(int i = 0;i < 3;i++)
    {
        for(int j = 0;j < 3;j++)
        {
            real Value = 0;
          
            for(int k = 0;k < 3;k++)
            {
                Value += Multiplicand.GetElement(i,k) *
                        Multiplier.GetElement(k,j);
            }

            ReturnMatrix.SetElement(i,j,Value);
        }
    }

    return ReturnMatrix;
}

vector_3 operator*( matrix_3x3 const &Multiplicand,
        vector_3 const &Multiplier )
{
    vector_3 ReturnPoint;

    for(int i = 0;i < 3;i++)
    {
        real Value = 0;
      
        for(int k = 0;k < 3;k++)
        {
            Value += Multiplicand.GetElement(i,k) *
                    Multiplier.GetElement(k);
        }

        ReturnPoint.SetElement(i,Value);
    }

    return ReturnPoint;
}


/*----------------------------------------------------------------------------

constructors.

*/

matrix_3x3::matrix_3x3( void )
{
    for(int Counter = 0;Counter < 9;Counter++)
    {
        aElements[0][Counter] = 0;
    }

    aElements[0][0] = aElements[1][1] = aElements[2][2] = 1;
}

/*----------------------------------------------------------------------------

vector length and normalize.

*/

real vector_3::Length( void ) const
{
    return sqrt(GetX()*GetX() + GetY()*GetY() + GetZ()*GetZ());
}

vector_3 &vector_3::Normalize( void )
{
    real MyLength = Length();

    if(MyLength != 0)
    {
        SetX(GetX() / MyLength);
        SetY(GetY() / MyLength);
        SetZ(GetZ() / MyLength);
    }

    return *this;
}
  
void OrthonormalizeOrientation( matrix_3x3 &Orientation )
{
    vector_3 X(Orientation(0,0),Orientation(1,0),Orientation(2,0));
    vector_3 Y(Orientation(0,1),Orientation(1,1),Orientation(2,1));
    vector_3 Z;

    X.Normalize();
    Z = CrossProduct(X,Y).Normalize();
    Y = CrossProduct(Z,X).Normalize();

    Orientation(0,0) = X(0); Orientation(0,1) = Y(0); Orientation(0,2) = Z(0);
    Orientation(1,0) = X(1); Orientation(1,1) = Y(1); Orientation(1,2) = Z(1);
    Orientation(2,0) = X(2); Orientation(2,1) = Y(2); Orientation(2,2) = Z(2);
}



//*** Turn on float conversion warning
#pragma warning ( default : 4136 )
#pragma warning ( disable : 4514 ) // unused inline function

