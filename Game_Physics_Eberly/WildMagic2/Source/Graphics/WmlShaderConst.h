// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

// ShaderConst is more or less a "struct" class that abstracts register
// numbers away from the user and lets them deal with objects instead.  It is
// usually contained and gotten by the user from some ShaderConstants object.

#ifndef WMLSHADERCONST
#define WMLSHADERCONST

#include "WmlStateConstant.h"
#include "WmlObject.h"
#include "WmlVector2.h"
#include "WmlVector3.h"
#include "WmlVector4.h"
#include "WmlMatrix4.h"
#include <iostream>

namespace Wml
{

class WML_ITEM ShaderConst : public Object
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // constructors and destructor
    ShaderConst ();
    ShaderConst (const char* acConstantName, int iRegister, int iSize,
        StateConstantType iType, int iTypeOption);
    ShaderConst (const ShaderConst* pkShaderConst);
    ShaderConst (const ShaderConst& rkConst);
    virtual ~ShaderConst ();

    // assignment
    ShaderConst& operator= (const ShaderConst& rkConst);

    // member access
    int GetRegister () const;
    const float* GetData () const;
    float* GetData ();
    int GetSize () const; // size is in terms of registers (four floats)
    StateConstantType GetType () const;
    int GetTypeOption () const;

    void SetRegister (int iRegister);
    void SetData (const float* afData);
    void SetData (const Vector2f& rkData);
    void SetData (const Vector3f& rkData);
    void SetData (const Vector4f& rkData);
    void SetData (const Matrix4f& rkData);
    void SetData (float fX, float fY = 0.0f, float fZ = 0.0f,
        float fW = 1.0f);
    void SetSize (int iSize);  // if called, invalidates previous data
    void SetType (StateConstantType iType);
    void SetTypeOption (int iTypeOption);

    bool NameMatches (const char* acConstantName) const;

    // TO DO.  Are these here for streaming?
    void Write (std::ostream& rkOStr);
    void Read (std::istream& rkIStr);

protected:
    int m_iRegister;
    float* m_afData;
    int m_iSize;
    StateConstantType m_iType;
    int m_iTypeOption;
    char* m_acConstantName;
};

WmlSmartPointer(ShaderConst);
WmlRegisterStream(ShaderConst);

#include "WmlShaderConst.inl"

}

#endif
