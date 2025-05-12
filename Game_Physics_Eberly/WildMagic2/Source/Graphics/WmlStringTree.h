// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLSTRINGTREE_H
#define WMLSTRINGTREE_H

#include "WmlBound.h"
#include "WmlColorRGB.h"
#include "WmlMatrix2.h"
#include "WmlMatrix3.h"
#include "WmlQuaternion.h"
#include "WmlRTTI.h"
#include "WmlVector2.h"
#include "WmlVector3.h"
#include <vector>

namespace Wml
{

class WML_ITEM StringTree
{
public:
    // construction and destruction
    StringTree (int iStringQuantity, int iStringGrowBy, int iChildQuantity,
        int iChildGrowBy);

    ~StringTree ();

    // strings
    void SetStringQuantity (int iQuantity);
    int GetStringQuantity () const;
    char* SetString (int i, char* acString);
    char* GetString (int i);

    // children
    void SetChildQuantity (int iQuantity);
    int GetChildQuantity () const;
    StringTree* SetChild (int i, StringTree* pkChild);
    StringTree* GetChild (int i);

    // streaming
    bool Save (const char* acFilename, int iTabSize = 4);

protected:
    // streaming (recursive)
    void Save (FILE* pkOFile, int iLevel, int iTabSize);

    // node data
    std::vector<char*> m_kStrings;
    int m_iStringGrowBy;

    // children
    std::vector<StringTree*> m_kChildren;
    int m_iChildGrowBy;
};

// string creation helpers (native types)
WML_ITEM char* MakeString (const RTTI* pkRTTI, const char* acName);
WML_ITEM char* MakeString (const char* acString);
WML_ITEM char* MakeString (const char* acPrefix, bool bValue);
WML_ITEM char* MakeString (const char* acPrefix, char cValue);
WML_ITEM char* MakeString (const char* acPrefix, unsigned char ucValue);
WML_ITEM char* MakeString (const char* acPrefix, short sValue);
WML_ITEM char* MakeString (const char* acPrefix, unsigned short usValue);
WML_ITEM char* MakeString (const char* acPrefix, int iValue);
WML_ITEM char* MakeString (const char* acPrefix, unsigned int uiValue);
WML_ITEM char* MakeString (const char* acPrefix, long lValue);
WML_ITEM char* MakeString (const char* acPrefix, unsigned long ulValue);
WML_ITEM char* MakeString (const char* acPrefix, float fValue);
WML_ITEM char* MakeString (const char* acPrefix, double dValue);
WML_ITEM char* MakeString (const char* acPrefix, void* pvValue);
WML_ITEM char* MakeString (const char* acPrefix, const char* acValue);
WML_ITEM char* MakeString (const char* acPrefix, const ColorRGB& rkValue);
WML_ITEM char* MakeString (const char* acPrefix, const Matrix3f& rkValue);
WML_ITEM char* MakeString (const char* acPrefix, const Quaternionf& rkValue);
WML_ITEM char* MakeString (const char* acPrefix, const Vector2f& rkValue);
WML_ITEM char* MakeString (const char* acPrefix, const Vector3f& rkValue);
WML_ITEM char* MakeString (const char* acPrefix, const Bound& rkValue);

#include "WmlStringTree.inl"

}

#endif


