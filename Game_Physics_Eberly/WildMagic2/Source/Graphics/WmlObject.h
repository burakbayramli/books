// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLOBJECT_H
#define WMLOBJECT_H

#include "WmlRTTI.h"
#include "WmlSmartPointer.h"
#include "WmlStream.h"

namespace Wml
{

WmlSmartPointer(Object);
class Controller;
typedef Object* (*FactoryFunction)(Stream&);

class WML_ITEM Object
{
    WmlDeclareRootRTTI;
    WmlDeclareRootSmartPointer;
    WmlDeclareRootStream;
    
public:
    // destruction
    virtual ~Object ();

    // names, support for searching by name
    void SetName (const char* acName);
    const char* GetName () const;
    virtual Object* GetObjectByName (const char* acName);
    virtual void GetAllObjectsByName (const char* acName,
        std::vector<Object*>& rkObjects);

    // unique identification
    unsigned int GetID () const;

    // controllers
    bool AttachControl (Controller* pkControl);
    bool DetachControl (Controller* pkControl);
    Controller* GetControllers ();

    // Total objects currently active.  Use this to track object leaks in an
    // application by calling this first in 'main' (or 'WinMain') and saving
    // the quantity.  Objects constructed pre-main will be counted in this
    // call.  At the end of 'main' (or 'WinMain'), call the function again
    // and compare to the quantity of the original call--the quantities should
    // be the same (still positive since post-main destruction has not yet
    // occurred).
    unsigned int GetTotalObjects ();

protected:
    // construction (abstract base class)
    Object ();

    // identification
    char* m_acName;
    unsigned int m_uiID;
    static unsigned int ms_uiNextID;

    // controllers
    ObjectPtr m_spkControl;

private:
    // total objects currently active
    static unsigned int ms_uiTotalObjects;
};

#include "WmlObject.inl"

}

#endif


