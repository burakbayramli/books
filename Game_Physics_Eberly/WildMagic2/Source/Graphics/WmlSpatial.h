// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLSPATIAL_H
#define WMLSPATIAL_H

#include "WmlBound.h"
#include "WmlMatrix3.h"
#include "WmlObject.h"
#include "WmlRenderState.h"
#include <vector>

namespace Wml
{

class Renderer;
class Node;
WmlSmartPointer(Spatial);

class WML_ITEM Spatial : public Object
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // local transform access
    Matrix3f& Rotate ();
    const Matrix3f& Rotate () const;
    Vector3f& Translate ();
    const Vector3f& Translate () const;
    float& Scale ();
    const float& Scale () const;

    // world transform access ('set' should be used only by controllers)
    Matrix3f& WorldRotate ();
    const Matrix3f& WorldRotate () const;
    Vector3f& WorldTranslate ();
    const Vector3f& WorldTranslate () const;
    float& WorldScale ();
    const float& WorldScale () const;
    void SetWorldTransformToIdentity ();

    // world bound access
    Bound& WorldBound ();
    const Bound& WorldBound () const;

    // culling
    bool& ForceCull ();
    const bool& ForceCull () const;

    // render state
    RenderStatePtr SetRenderState (RenderState* pkState);
    RenderStatePtr GetRenderState (RenderState::Type eType);
    RenderStatePtr RemoveRenderState (RenderState::Type eType);
    void RemoveAllStates ();

    // updates (GS = geometric state, RS = renderer state)
    void UpdateGS (float fAppTime, bool bInitiator = true);
    void UpdateRS (RenderState::Stack* = NULL);

    // parent access
    Node* GetParent ();

    // support for searching by name
    virtual Object* GetObjectByName (const char* acName);
    virtual void GetAllObjectsByName (const char* acName,
        std::vector<Object*>& rkObjects);

    // The picking system.  Each Spatial-derived class derives its own pick
    // record class from PickRecord and adds whatever information it wants to
    // return from the DoPick call.  The ray parameter can be used to sort
    // the intersection points after a call to DoPick.
    //
    // RTTI for the PickRecord is obtained by using the RTTI for the member
    // m_spkObject.  Once that member's type is known, the PickRecord can be
    // cast to the appropriate PickRecord-derived class.
    class WML_ITEM PickRecord
    {
    public:
        virtual ~PickRecord ();

        // The intersected object.
        SpatialPtr m_spkObject;

        // The world ray parameter for the point of intersection, a
        // nonnegative number.  That is, if the world ray is P+t*D, the ray
        // parameter is t >= 0.
        float m_fRayT;

        // For sorting in ascending order of the ray parameter.
        bool operator< (const PickRecord& rkRecord) const;

    protected:
        PickRecord (Spatial* pkObject, float fRayT);

    };

    typedef std::vector<PickRecord*> PickArray;

    // The origin and direction of the ray must be in world coordinates.  The
    // application is responsible for deleting the pick records in the array.
    virtual void DoPick (const Vector3f& rkOrigin,
        const Vector3f& rkDirection, PickArray& rkResults);

    // sort the pick records in ascending order of the ray parameter
    static void Sort (PickArray& rkResults);

    // For drawing by the rendering system.  These are only public to allow
    // render effect objects to fall back to standard drawing if the effects
    // are not supported by the renderer.
    void OnDraw (Renderer& rkRenderer);
    virtual void Draw (Renderer& rkRenderer) = 0;

protected:
    // construction (abstract base class)
    Spatial ();
    virtual ~Spatial ();

    // parent access (Node calls this during attach/detach of children)
    friend class Node;
    void SetParent (Node* pkParent);

    // geometric updates
    virtual void UpdateWorldData (float fAppTime);
    virtual void UpdateWorldBound () = 0;
    void PropagateBoundToRoot ();

    // render state updates
    virtual void UpdateRenderState (RenderState::Stack* pkStack) = 0;
    void PropagateStateFromRoot (RenderState::Stack* pkStack);
    void RestoreStateToRoot (RenderState::Stack* pkStack);

    // parents
    Node* m_pkParent;

    // local transforms
    Matrix3f m_kRotate;
    Vector3f m_kTranslate;
    float m_fScale;

    // world transforms
    Matrix3f m_kWorldRotate;
    Vector3f m_kWorldTranslate;
    float m_fWorldScale;

    // world bound
    Bound m_kWorldBound;

    // culling
    bool m_bForceCull;

    // render state
    RenderState::List* m_pkStateList;
};

WmlRegisterStream(Spatial);
#include "WmlSpatial.inl"

}

#endif


