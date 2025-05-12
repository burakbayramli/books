// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlCollisionGroup.h"
#include "WmlCollisionRecord.h"
using namespace Wml;

//----------------------------------------------------------------------------
CollisionGroup::CollisionGroup ()
{
}
//----------------------------------------------------------------------------
CollisionGroup::~CollisionGroup ()
{
    for (int i = 0; i < (int)m_kRArray.size(); i++)
        delete m_kRArray[i];
}
//----------------------------------------------------------------------------
bool CollisionGroup::Add (CollisionRecord* pkRecord)
{
    for (int i = 0; i < (int)m_kRArray.size(); i++)
    {
        if ( pkRecord == m_kRArray[i] )
            return false;
    }

    m_kRArray.push_back(pkRecord);
    return true;
}
//----------------------------------------------------------------------------
bool CollisionGroup::Remove (CollisionRecord* pkRecord)
{
    for (Iterator pkI = m_kRArray.begin(); pkI != m_kRArray.end(); pkI++)
    {
        if ( pkRecord == *pkI)
        {
            m_kRArray.erase(pkI);
            delete pkRecord;
            return true;
        }
    }

    return false;
}
//----------------------------------------------------------------------------
void CollisionGroup::TestIntersection ()
{
    // objects are assumed to be stationary, compare all pairs
    for (int i0 = 0; i0 < (int)m_kRArray.size(); i0++)
    {
        CollisionRecord* pkRecord0 = m_kRArray[i0];

        for (int i1 = i0+1; i1 < (int)m_kRArray.size(); i1++)
        {
            CollisionRecord* pkRecord1 = m_kRArray[i1];

            pkRecord0->Initialize();
            pkRecord1->Initialize();
            pkRecord0->TestIntersection(*pkRecord1);
        }
    }
}
//----------------------------------------------------------------------------
void CollisionGroup::FindIntersection ()
{
    // objects are assumed to be stationary, compare all pairs
    for (int i0 = 0; i0 < (int)m_kRArray.size(); i0++)
    {
        CollisionRecord* pkRecord0 = m_kRArray[i0];

        for (int i1 = i0+1; i1 < (int)m_kRArray.size(); i1++)
        {
            CollisionRecord* pkRecord1 = m_kRArray[i1];

            pkRecord0->Initialize();
            pkRecord1->Initialize();
            pkRecord0->FindIntersection(*pkRecord1);
        }
    }
}
//----------------------------------------------------------------------------
void CollisionGroup::TestIntersection (float fTMax)
{
    // objects are assumed to be stationary, compare all pairs
    for (int i0 = 0; i0 < (int)m_kRArray.size(); i0++)
    {
        CollisionRecord* pkRecord0 = m_kRArray[i0];

        for (int i1 = i0+1; i1 < (int)m_kRArray.size(); i1++)
        {
            CollisionRecord* pkRecord1 = m_kRArray[i1];

            if ( pkRecord0->GetVelocity() || pkRecord1->GetVelocity() )
            {
                pkRecord0->Initialize();
                pkRecord1->Initialize();
                pkRecord0->TestIntersection(fTMax,*pkRecord1);
            }
        }
    }
}
//----------------------------------------------------------------------------
void CollisionGroup::FindIntersection (float fTMax)
{
    // objects are assumed to be stationary, compare all pairs
    for (int i0 = 0; i0 < (int)m_kRArray.size(); i0++)
    {
        CollisionRecord* pkRecord0 = m_kRArray[i0];

        for (int i1 = i0+1; i1 < (int)m_kRArray.size(); i1++)
        {
            CollisionRecord* pkRecord1 = m_kRArray[i1];

            if ( pkRecord0->GetVelocity() || pkRecord1->GetVelocity() )
            {
                pkRecord0->Initialize();
                pkRecord1->Initialize();
                pkRecord0->FindIntersection(fTMax,*pkRecord1);
            }
        }
    }
}
//----------------------------------------------------------------------------
