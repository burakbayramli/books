// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLNORMALCOMPRESSION_H
#define WMLNORMALCOMPRESSION_H

#include "WmlSystem.h"

namespace Wml
{

// Compress unit-length normal vectors (x,y,z) to 16-bit quantities.  3 bits
// are used to identify the octant containing the vector, the other 13 bits
// are used for the mantissa.

WML_ITEM void CompressNormal (double dX, double dY, double dZ,
    unsigned short& rusIndex);

WML_ITEM void UncompressNormal (unsigned short usIndex, double& rdX,
    double& rdY, double& rdZ);

// An example of how to use the compression.  This shows the maximum error is
// about 10-degrees between the original and the compressed-then-uncompressed
// vector.
//
//void TestCompression ()
//{
//    const int iS = 1024;
//    double dDotMin = 1.0;
//    int iXMin = iS, iYMin = iS;
//
//    for (int iY = 0; iY < iS; iY++)
//    {
//        double dY0 = iY/(double)iS;
//        for (int iX = 0; iX < iS; iX++)
//        {
//            double dX0 = iX/(double)iS;
//            double dZ0 = 1.0 - dX0*dX0 - dY0*dY0;
//            if ( dZ0 >= 0.0 )
//            {
//                dZ0 = sqrt(dZ0);
//
//                unsigned short usIndex;
//                CompressNormal(dX0,dY0,dZ0,usIndex);
//                assert( usIndex < 8192 );
//
//                double dX1, dY1, dZ1;
//                UncompressNormal(usIndex,dX1,dY1,dZ1);
//
//                double dDot = dX0*dX1+dY0*dY1+dZ0*dZ1;
//                if ( dDot < dDotMin )
//                {
//                    dDotMin = dDot;
//                    iXMin = iX;
//                    iYMin = iY;
//                }
//            }
//        }
//    }
//
//    // S = 16384, dotmin = 0.98474228151906 (about 10-degrees error)
//}
}

#endif
