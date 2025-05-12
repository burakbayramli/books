// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCONTROLLER_H
#define WMLCONTROLLER_H

#include "WmlObject.h"

namespace Wml
{

WmlSmartPointer(Controller);

class WML_ITEM Controller : public Object
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    virtual ~Controller ();

    enum RepeatType
    {
        RT_CLAMP,
        RT_WRAP,
        RT_CYCLE,
        RT_QUANTITY
    };

    RepeatType& Repeat ();

    float& MinTime ();
    float& MaxTime ();
    float& Phase ();
    float& Frequency ();

    bool& Active ();
    bool Active () const;

    virtual void SetObject (Object* pkObject);
    Object* GetObject () const;
    void SetNext (Controller* pkNext);
    Controller* GetNext () const;

    virtual bool Update (float fAppTime) = 0;

    // support for searching by name
    virtual Object* GetObjectByName (const char* acName);
    virtual void GetAllObjectsByName (const char* acName,
        std::vector<Object*>& rkObjects);

protected:
    // abstract base class
    Controller ();

    // Conversion from application time units to controller time units.
    // Derived classes may use this in their update routines.
    float GetControlTime (float fAppTime);

    RepeatType m_eRepeat;
    float m_fMinTime, m_fMaxTime, m_fPhase, m_fFrequency;
    Object* m_pkObject;
    ControllerPtr m_spkNext;
    bool m_bActive;

    // Object is a friend for internal list handling.  It needs access to
    // m_pkObject and m_spkNext.
    friend class Object;
};

WmlRegisterStream(Controller);
#include "WmlController.inl"

}

#endif


