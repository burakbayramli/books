// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlSkinController.h"
#include "WmlGeometry.h"
#include "WmlNode.h"
#include <string>
using namespace Wml;
using namespace std;

WmlImplementRTTI(SkinController,Controller);
WmlImplementStream(SkinController);

//----------------------------------------------------------------------------
SkinController::SkinController ()
{
    m_iBoneQuantity = 0;
    m_apkBone = NULL;
    m_aiSkinVertexQuantity = NULL;
    m_aakSkinVertex = NULL;
}
//----------------------------------------------------------------------------
SkinController::~SkinController ()
{
    delete[] m_apkBone;
    delete[] m_aiSkinVertexQuantity;

    for (int i = 0; i < m_iBoneQuantity; i++)
        delete[] m_aakSkinVertex[i];

    delete[] m_aakSkinVertex;
}
//----------------------------------------------------------------------------
bool SkinController::Update (float)
{
    // The skin vertices are calculated in the bone world coordinate system.
    // The bone world coordinates already includes the world transform of the
    // common parent, so the world transforms for the skin mesh should be
    // the identity.
    Geometry* pkGeom = (Geometry*) m_pkObject;
    pkGeom->SetWorldTransformToIdentity();

    if ( !Active() )
        return true;

    // set all vertices to the zero vector
    Vector3f* akVertex = pkGeom->Vertices();
    memset(akVertex,0,pkGeom->GetVertexQuantity()*sizeof(Vector3f));


    // update dependent vertices for each bone
    for (int i = 0; i < m_iBoneQuantity; i++)
    {
        Node* pkBone = m_apkBone[i];
        Matrix3f kSRot = pkBone->WorldScale()*pkBone->WorldRotate();
        int iVQuantity = m_aiSkinVertexQuantity[i];
        SkinVertex* pkSV = m_aakSkinVertex[i];
        for (int j = 0; j < iVQuantity; j++, pkSV++)
        {
            Vector3f kVTrn = kSRot*pkSV->m_kOffset + pkBone->WorldTranslate();
            akVertex[pkSV->m_iIndex] += pkSV->m_fWeight*kVTrn;
        }
    }

    // update vertex normals if the skin has them
    if ( pkGeom->Normals() )
        pkGeom->UpdateModelNormals();

    pkGeom->UpdateModelBound();

    // controller computes world transform
    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* SkinController::Factory (Stream& rkStream)
{
    SkinController* pkObject = new SkinController;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void SkinController::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Controller::Load(rkStream,pkLink);

    StreamRead(rkStream,m_iBoneQuantity);

    m_apkBone = new Node*[m_iBoneQuantity];
    StreamRead(rkStream,m_apkBone,m_iBoneQuantity);
    int i;
    for (i = 0; i < m_iBoneQuantity; i++)
        pkLink->Add(m_apkBone[i]);

    m_aiSkinVertexQuantity = new int[m_iBoneQuantity];
    StreamRead(rkStream,m_aiSkinVertexQuantity,m_iBoneQuantity);

    m_aakSkinVertex = new SkinVertex*[m_iBoneQuantity];
    for (i = 0; i < m_iBoneQuantity; i++)
    {
        int iVQuantity = m_aiSkinVertexQuantity[i];
        m_aakSkinVertex[i] = new SkinVertex[iVQuantity];
        SkinVertex* pkSV = m_aakSkinVertex[i];
        for (int j = 0; j < iVQuantity; j++, pkSV++)
        {
            StreamRead(rkStream,pkSV->m_iIndex);
            StreamRead(rkStream,pkSV->m_fWeight);
            StreamRead(rkStream,pkSV->m_kOffset);
        }
    }
}
//----------------------------------------------------------------------------
void SkinController::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Controller::Link(rkStream,pkLink);

    for (int i = 0; i < m_iBoneQuantity; i++)
    {
        Object* pkLinkID = pkLink->GetLinkID();
        m_apkBone[i] = (Node*)rkStream.GetFromMap(pkLinkID);
    }
}
//----------------------------------------------------------------------------
bool SkinController::Register (Stream& rkStream)
{
    if ( !Controller::Register(rkStream) )
        return false;

    for (int i = 0; i < m_iBoneQuantity; i++)
        m_apkBone[i]->Register(rkStream);

    return true;
}
//----------------------------------------------------------------------------
void SkinController::Save (Stream& rkStream)
{
    Controller::Save(rkStream);

    StreamWrite(rkStream,m_iBoneQuantity);
    StreamWrite(rkStream,m_apkBone,m_iBoneQuantity);
    StreamWrite(rkStream,m_aiSkinVertexQuantity,m_iBoneQuantity);

    for (int i = 0; i < m_iBoneQuantity; i++)
    {
        int iVQuantity = m_aiSkinVertexQuantity[i];
        SkinVertex* pkSV = m_aakSkinVertex[i];
        for (int j = 0; j < iVQuantity; j++, pkSV++)
        {
            StreamWrite(rkStream,pkSV->m_iIndex);
            StreamWrite(rkStream,pkSV->m_fWeight);
            StreamWrite(rkStream,pkSV->m_kOffset);
        }
    }
}
//----------------------------------------------------------------------------
StringTree* SkinController::SaveStrings ()
{
    // TO DO.  Finish implementation.
    StringTree* pkTree = new StringTree(2,0,2,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("bone quantity = ",m_iBoneQuantity));

    // children
    pkTree->SetChild(0,Controller::SaveStrings());

    StringTree* pkAttrTree = new StringTree(m_iBoneQuantity+1,0,0,0);
    pkTree->SetChild(1,pkAttrTree);
    pkAttrTree->SetString(0,MakeString("bones"));
    for (int i = 0; i < m_iBoneQuantity; i++)
    {
        char acSubfield[256];
        string kField;

        // bone name (if known) or pointer
        if ( m_apkBone[i]->GetName() )
        {
            kField.append("<");
            kField.append(m_apkBone[i]->GetName());
            kField.append(">");
        }
        else
        {
            sprintf(acSubfield,"%p",m_apkBone[i]);
            kField.append(acSubfield);
        }

        // count of vertices affected by bone
        sprintf(acSubfield," (vertices influenced = %d)",
            m_aiSkinVertexQuantity[i]);
        kField.append(acSubfield);

        // make a copy of the field to attach to the attribute tree
        char* acField = new char[strlen(kField.c_str())+1];
        strcpy(acField,kField.c_str());
        pkAttrTree->SetString(i+1,acField);
    }

    return pkTree;
}
//----------------------------------------------------------------------------
int SkinController::GetMemoryUsed () const
{
    int iBaseSize = sizeof(SkinController) - sizeof(Controller);

    int iDynaSize = m_iBoneQuantity*(sizeof(m_apkBone[0]) +
        sizeof(m_aiSkinVertexQuantity[0]));

    for (int i = 0; i < m_iBoneQuantity; i++)
    {
        iDynaSize += m_iBoneQuantity*m_aiSkinVertexQuantity[i]*
            sizeof(SkinVertex);
    }

    int iTotalSize = iBaseSize + iDynaSize + Controller::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int SkinController::GetDiskUsed () const
{
    int iSize = Controller::GetDiskUsed() +
        sizeof(m_iBoneQuantity) +
        m_iBoneQuantity*sizeof(m_apkBone[0]) +
        m_iBoneQuantity*sizeof(m_aiSkinVertexQuantity[0]);

    for (int i = 0; i < m_iBoneQuantity; i++)
    {
        int iVQuantity = m_aiSkinVertexQuantity[i];
        SkinVertex* pkSV = m_aakSkinVertex[i];
        for (int j = 0; j < iVQuantity; j++, pkSV++)
        {
            iSize += sizeof(pkSV->m_iIndex);
            iSize += sizeof(pkSV->m_fWeight);
            iSize += sizeof(pkSV->m_kOffset);
        }
    }

    return iSize;
}
//----------------------------------------------------------------------------
