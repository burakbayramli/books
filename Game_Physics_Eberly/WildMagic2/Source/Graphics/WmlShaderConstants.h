// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

// ShaderConstants is essentially a table of the information that a renderer
// will need to set user-defined constants.  Each register entry has a
// register number, a pointer to data, and the size of that data.  The other
// two pieces of information that it can have

#ifndef WMLSHADERCONSTANTS
#define WMLSHADERCONSTANTS

#include "WmlShaderConst.h"
#include <vector>

namespace Wml
{

class Renderer;

class WML_ITEM ShaderConstants : public Object
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // construction and destruction
    ShaderConstants ();
    ShaderConstants (const ShaderConstants& rkConstants);
    virtual ~ShaderConstants ();

    void AddConstant (const char* acConstantName, int iRegister,
        int iSize, StateConstantType iType, int iTypeOption);
    void AddConstant (const char* acConstantName, int iRegister,
        int iSize, StateConstantType iType, int iTypeOption,
        const float* afInitialData);
    void AddConstant (const char* acConstantName, int iRegister,
        int iSize, StateConstantType iType, int iTypeOption,
        const double* adInitialData);

    // returns null if constant not found
    ShaderConstPtr GetConstant (const char* acName) const;

    // return by row in the vector (internal use)
    ShaderConstPtr GetConstant (int iConstantNum) const;

    int GetNumConstants () const;

    // TO DO.  Are these here for streaming?
    void Write (std::ostream& rkOStr);
    void Read (std::istream& rkIStr);

protected:
    std::vector<ShaderConstPtr> m_kConstants;
};

WmlSmartPointer(ShaderConstants);
WmlRegisterStream(ShaderConstants);

#include "WmlShaderConstants.inl"

}

#endif
