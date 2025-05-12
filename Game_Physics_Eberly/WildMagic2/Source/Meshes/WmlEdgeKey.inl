// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

//----------------------------------------------------------------------------
inline EdgeKey::EdgeKey (int iV0, int iV1)
{
    if ( iV0 < iV1 )
    {
        // v0 is minimum
        V[0] = iV0;
        V[1] = iV1;
    }
    else
    {
        // v1 is minimum
        V[0] = iV1;
        V[1] = iV0;
    }
}
//----------------------------------------------------------------------------
inline bool EdgeKey::operator< (const EdgeKey& rkKey) const
{
    if ( V[1] < rkKey.V[1] )
        return true;
    if ( V[1] > rkKey.V[1] )
        return false;
    return V[0] < rkKey.V[0];
}
//----------------------------------------------------------------------------
