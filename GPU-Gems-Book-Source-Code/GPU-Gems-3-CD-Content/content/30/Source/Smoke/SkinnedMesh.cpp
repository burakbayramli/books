//----------------------------------------------------------------------------------
// File:   SkinnedMesh.cpp
// Author: Ignacio Llamas
// Email:  sdkfeedback@nvidia.com
// 
// Copyright (c) 2007 NVIDIA Corporation. All rights reserved.
//
// TO  THE MAXIMUM  EXTENT PERMITTED  BY APPLICABLE  LAW, THIS SOFTWARE  IS PROVIDED
// *AS IS*  AND NVIDIA AND  ITS SUPPLIERS DISCLAIM  ALL WARRANTIES,  EITHER  EXPRESS
// OR IMPLIED, INCLUDING, BUT NOT LIMITED  TO, IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL  NVIDIA OR ITS SUPPLIERS
// BE  LIABLE  FOR  ANY  SPECIAL,  INCIDENTAL,  INDIRECT,  OR  CONSEQUENTIAL DAMAGES
// WHATSOEVER (INCLUDING, WITHOUT LIMITATION,  DAMAGES FOR LOSS OF BUSINESS PROFITS,
// BUSINESS INTERRUPTION, LOSS OF BUSINESS INFORMATION, OR ANY OTHER PECUNIARY LOSS)
// ARISING OUT OF THE  USE OF OR INABILITY  TO USE THIS SOFTWARE, EVEN IF NVIDIA HAS
// BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
//
//
//----------------------------------------------------------------------------------


//----------------------------------------------------------------------------------
// Includes
//----------------------------------------------------------------------------------
#include "Smoke.h"
#include "SkinnedMesh.h"
#include "nvutmedia.h"
#include "SDKmisc.h"
#include <assert.h>
#include <algorithm>

#include "FCDocument/FCDSceneNode.h"
#include "FCDocument/FCDEntityInstance.h"
#include "FCDocument/FCDGeometryInstance.h"
#include "FCDocument/FCDController.h"
#include "FCDocument/FCDSkinController.h"
#include "FCDocument/FCDControllerInstance.h"
#include "FCDocument/FCDGeometrySource.h"

#include "FCDocument/FCDMaterialInstance.h"
#include "FCDocument/FCDMaterial.h"
#include "FCDocument/FCDEffect.h"
#include "FCDocument/FCDEffectProfile.h"
#include "FCDocument/FCDEffectStandard.h"
#include "FCDocument/FCDEffectParameter.h"
#include "FCDocument/FCDEffectParameterList.h"
#include "FCDocument/FCDEffectParameterSurface.h"
#include "FCDocument/FCDImage.h"
#include "FCDocument/FCDTexture.h"
#include "FCDocument/FCDLibrary.h"

using namespace std;


//----------------------------------------------------------------------------------
// Class SkinnedMesh
//----------------------------------------------------------------------------------
SkinnedMesh::SkinnedMesh()
{
}

SkinnedMesh::~SkinnedMesh()
{
}

SkinnedMesh *SkinnedMesh::CreateD3D10FromColladaFile(LPCTSTR filename,
    ID3D10Device* pd3dDevice, int maxSoBufNum)
{
    SkinnedMeshD3D10 *newSkinnedMesh(NULL);
    newSkinnedMesh = new SkinnedMeshD3D10();
    if(!newSkinnedMesh)
        return NULL;

    if( !newSkinnedMesh->LoadFromColladaFile(filename) ||
        !newSkinnedMesh->InitializeForRendering(pd3dDevice, maxSoBufNum) )
    {
        SAFE_DELETE(newSkinnedMesh);
        return NULL;
    }


    return (SkinnedMesh*)newSkinnedMesh;
}
//----------------------------------------------------------------------------------
// Class SkinnedMeshImpl
//----------------------------------------------------------------------------------

SkinnedMeshImpl::SkinnedMeshImpl() : m_vtxCount(0), m_triCount(0), m_animEndHandling(LOOP),
    m_currentTime(0), m_startTime(0), m_endTime(0), m_minTime(0), m_maxTime(0),
    m_lastNodesUpdateTime(0)
{
    InitFsemToSemMap();
}

SkinnedMeshImpl::~SkinnedMeshImpl()
{
}


//////////////////////////////////////////////////////////////////////////////
// BEGIN Loading Code

bool SkinnedMeshImpl::LoadFromColladaFile(LPCTSTR filename)
{
    fstring fFileName(filename);

    FCDocument* document = FCollada::NewTopDocument();
    bool status = document->LoadFromFile(filename);
    if(!status)
        return false;

    // We need to have at least a visual scene and a geometry library
    FCDSceneNode* sceneRoot = document->GetVisualSceneRoot();
    FCDGeometryLibrary *geometries = document->GetGeometryLibrary();
    if(!sceneRoot || !geometries || geometries->IsEmpty() ) {
        return false;
    }

    // Reset some data before starting the actual load process
    m_minTime = FLT_MAX;
    m_maxTime = FLT_MIN;

    // Add the list of nodes in the visual scene assuming it's a tree (not a DAG)
    //   as the nodes are added any instance data (geometry, skinning controllers, etc)
    //   is added or referenced as needed
    (void)AddNode(sceneRoot, NULL);
    
    if(m_minTime != FLT_MAX)
    {
        m_startTime = m_minTime;
        m_endTime = m_maxTime;
    } else {
        assert(m_minTime == FLT_MAX);
        assert(m_maxTime == FLT_MIN);
        m_minTime = 0;
        m_maxTime = 0;
    }
    if(m_startTime < 0) 
        m_startTime = 0;
    m_lastNodesUpdateTime = FLT_MIN;

    SetCurrentTime(m_startTime);


    SAFE_DELETE(document);

    // Self-consistency check
    return IsSelfConsistent();
}

bool SkinnedMeshImpl::IsSelfConsistent(void)
{
    int numInconsistencies(0);

#define SELFCHECK_ASSERT( expr ) { if( ! (expr) ) { numInconsistencies++; assert(0); } }

    // all arrays in m_vtxAttributes should have m_vtxCount elements
    VtxAttributeArrayList::iterator vtxAttrIt(m_vtxAttributes.begin()), vtxAttrEnd(m_vtxAttributes.end());
    for(; vtxAttrIt != vtxAttrEnd; ++vtxAttrIt)
        SELFCHECK_ASSERT( m_vtxCount == vtxAttrIt->m_count );

    SELFCHECK_ASSERT( m_semToVtxAttrAr.size() == m_vtxAttributes.size() );

    SELFCHECK_ASSERT( m_indices.size() == m_triCount );
    IndexBuffer::iterator indexIt(m_indices.begin()), indexEnd(m_indices.end());
    for(; indexIt != indexEnd; ++indexIt)
        SELFCHECK_ASSERT( (*indexIt) < m_vtxCount );

    SELFCHECK_ASSERT( m_idToMesh.size() == m_meshes.size() );

    MeshList::iterator meshIt(m_meshes.begin()), meshEnd(m_meshes.end());
    size_t subsetCountSum(0);
    size_t vtxCountSum(0);
    for( ; meshIt != meshEnd; ++meshIt )
    {
        subsetCountSum += meshIt->subsets.size();
        SELFCHECK_ASSERT( vtxCountSum == meshIt->vtxBegin );
        vtxCountSum += meshIt->vtxCount;
    }
    SELFCHECK_ASSERT( subsetCountSum == m_meshSubsets.size() );
    SELFCHECK_ASSERT( vtxCountSum == m_vtxCount );

    MeshSubsetList::iterator subsetIt(m_meshSubsets.begin()), subsetEnd(m_meshSubsets.end());
    size_t idxCountSum(0);
    for( ; subsetIt != subsetEnd; ++subsetIt )
    {
        SELFCHECK_ASSERT( idxCountSum == subsetIt->idxBegin );
        idxCountSum += subsetIt->idxCount;
    }
    SELFCHECK_ASSERT( idxCountSum == m_indices.size() );

#undef SELFCHECK_ASSERT

    return (numInconsistencies == 0);
}


void SkinnedMeshImpl::InitFsemToSemMap(void)
{
    if(m_fsemToSemMap.size())
        return;

#define FSEMTOSEM_ENTRY( SEM ) m_fsemToSemMap[(uint32)FUDaeGeometryInput::SEM] = SkinnedMeshImpl::VtxAttributeArray::SEM;

    FSEMTOSEM_ENTRY(POSITION);
    FSEMTOSEM_ENTRY(VERTEX);
    FSEMTOSEM_ENTRY(NORMAL);
    FSEMTOSEM_ENTRY(GEOTANGENT);
    FSEMTOSEM_ENTRY(GEOBINORMAL);
    FSEMTOSEM_ENTRY(TEXCOORD);
    FSEMTOSEM_ENTRY(TEXTANGENT);
    FSEMTOSEM_ENTRY(TEXBINORMAL);
    FSEMTOSEM_ENTRY(UV);
    FSEMTOSEM_ENTRY(COLOR);
    FSEMTOSEM_ENTRY(EXTRA);
    FSEMTOSEM_ENTRY(UNKNOWN);

#undef FSEMTOSEM_ENTRY
}

SkinnedMeshImpl::Node * SkinnedMeshImpl::AddNode(FCDSceneNode *pSrcSceneNode, SkinnedMeshImpl::Node *pParentNode)
{
    assert(pSrcSceneNode);

    if(!ProcessNode(pSrcSceneNode, pParentNode))
        return NULL;

    // if ProcessNode succeeds the added node must exist at the end of m_nodes
    Node &nodeRef = m_nodes.back();
    
    // NOTE: for now assume each node can only have a parent, although COLLADA seems to support more
    nodeRef.id = pSrcSceneNode->GetDaeId();
    nodeRef.sid = pSrcSceneNode->GetSubId();
    nodeRef.pParent = pParentNode;
    
    FCDSceneNodeTrackList& children = pSrcSceneNode->GetChildren();
    FCDSceneNodeTrackList::iterator childIt(children.begin()), childEnd(children.end());
    for(; childIt != childEnd; ++childIt)
    {
        Node * child = AddNode(*childIt, &nodeRef);
        if(child) {
            nodeRef.children.push_back(child);
        }
    }

    // fill in the local matrix
    //memcpy(&nodeRef.xform.m, &pSrcSceneNode->CalculateLocalTransform().m, sizeof(nodeRef.xform));
    nodeRef.xform = pSrcSceneNode->CalculateLocalTransform();

    // get the sampled animation
    FloatList keyList;
    FMMatrix44List matrixList;

    pSrcSceneNode->GenerateSampledMatrixAnimation(keyList, matrixList);
    assert(keyList.size() == matrixList.size());

    // get minKeyTime and maxKeyTime
    size_t keyCount = min(keyList.size(), matrixList.size());
    float minKeyTime(FLT_MAX), maxKeyTime(FLT_MIN), keyTime;
    for(size_t keyIdx(0); keyIdx<keyCount; keyIdx++)
    {
        keyTime = keyList[keyIdx];
        minKeyTime = min(minKeyTime, keyTime);
        maxKeyTime = max(maxKeyTime, keyTime);
    }
    nodeRef.minKeyTime = minKeyTime;
    nodeRef.maxKeyTime = maxKeyTime;

#ifdef FIXED_SAMPLING_RATE
    // clear the sampled animation and get it again with a higher sampling rate
    keyList.clear();
    matrixList.clear();
    float samplingRate = 1.0/30.0f; // sample the animation at 30 fps
    for(float t=minKeyTime; t<maxKeyTime; t += samplingRate)
        keyList.push_back(t);
    size_t prevKeyListSize = keyList.size();
    pSrcSceneNode->GenerateSampledMatrixAnimation(keyList, matrixList);
    assert(keyList.size() >= prevKeyListSize);
    assert(keyList.size() == matrixList.size());
#endif
    
    keyCount = min(keyList.size(), matrixList.size());
    Matrix44 tmpXForm;
    for(size_t keyIdx(0); keyIdx<keyCount; keyIdx++)
    {
        keyTime = keyList[keyIdx];
        nodeRef.xformKeyFrames[keyTime] = matrixList[keyIdx];
    }
    

    // update global animation extremes
    m_minTime = min(m_minTime, minKeyTime);
    m_maxTime = max(m_maxTime, maxKeyTime);

    // save a pointer to our Node representation 
    //   for later use in ProcessControllerInstNode
    pSrcSceneNode->SetUserHandle(&nodeRef);

    return &nodeRef;
}

bool SkinnedMeshImpl::ProcessNode(FCDSceneNode *pSrcSceneNode, SkinnedMeshImpl::Node *pParentNode)
{
    assert(pSrcSceneNode);

    // process the instances in this node "adding" or "referencing" GEOMETRY and CONTROLLERS as needed
    int usefulInstanceCount(0);
    
    m_nodes.push_back(Node());
    Node *pNewNode(&(m_nodes.back()));

    // check the instances inside this node for those that we want
    FCDEntityInstanceContainer& instances = pSrcSceneNode->GetInstances();
    FCDEntityInstanceContainer::iterator instIt(instances.begin()), instEnd(instances.end());
    for(; instIt != instEnd; ++instIt)
    {
        FCDEntity::Type type = (*instIt)->GetEntityType();
        // Useful nodes for us are: 
        //  GEOMETRY and CONTROLLER (geometry with skinning or morph controllers)
        if( (type == FCDEntity::GEOMETRY) || (type == FCDEntity::CONTROLLER) )
        {
            bool usefulInstance = false;

            if( type == FCDEntity::GEOMETRY )
                usefulInstance = ProcessGeometryInstNode(*instIt, pNewNode);
            else if( type == FCDEntity::CONTROLLER )
                usefulInstance = ProcessControllerInstNode(*instIt, pNewNode);
            
            if(usefulInstance)
                usefulInstanceCount++;
        }
    }

    // if it has instantiated objects there are only a few types we want to import
    if(usefulInstanceCount == 0)
    {
        // if the node has no parent it's likely the root of the visual scene, so we want it
        // if the node is a joint or has children, we also want it even if it has no instances
        // (the children nodes may be useful)
        if(!pParentNode || pSrcSceneNode->IsJoint() || pSrcSceneNode->GetChildrenCount() )
        {
            return true;
        }
        // remove the node we added
        m_nodes.pop_back();
        return false;
    }

    return true;
}

bool SkinnedMeshImpl::ProcessGeometryInstNode(FCDEntityInstance *pEntityInstance, Node *pDstNode)
{
    assert(pEntityInstance);
    assert(pDstNode);

    FCDGeometryInstance *pGeomInst = dynamic_cast<FCDGeometryInstance*>(pEntityInstance);
    assert(pGeomInst);

    FCDEntity* pEntity = pGeomInst->GetEntity();

    Mesh *pMesh = GetGeometry(pEntity);
    
    if(!pMesh)
        return false;

    pDstNode->instances.push_back(MeshInstance());
    MeshInstance& instance = pDstNode->instances.back();
    instance.pMesh = pMesh;
    GenSubsetToMaterialMap(instance.subsetToMaterial, pMesh->subsets, pGeomInst);

    return true;
}

bool SkinnedMeshImpl::ProcessControllerInstNode(FCDEntityInstance *pEntityInstance, Node *pDstNode)
{
    assert(pEntityInstance);
    assert(pDstNode);

    FCDControllerInstance *pContInst = dynamic_cast<FCDControllerInstance*>(pEntityInstance);
    assert(pContInst);

    FCDController *pCont = dynamic_cast<FCDController*>(pContInst->GetEntity());

    if(!pCont->IsSkin())
        // TODO: we only support skin controllers for now
        return false;

    if(pCont->GetBaseTarget()->GetType() == FCDEntity::CONTROLLER)
        // TODO: we currently don't support chained controllers
        return false;

    FCDSkinController *pSkin = pCont->GetSkinController();
    FCDGeometry *pGeom = pCont->GetBaseGeometry();

    Mesh *pMesh = GetGeometry(pGeom);
    
    if(!pMesh)
        return false;


    if(pMesh->hasSkin)
        // TODO: we currently don't support binding of the same mesh to multiple controllers
        return false;

    pDstNode->instances.push_back(MeshInstance());
    MeshInstance& instance = pDstNode->instances.back();
    instance.pMesh = pMesh;
    GenSubsetToMaterialMap(instance.subsetToMaterial, pMesh->subsets, pContInst);
    

    // Get the bone data (bind poses and ptr to Node)
    //---------------------------------------------------
    assert(pContInst->GetJointCount() == pSkin->GetJointCount());
    size_t boneCount = pContInst->GetJointCount();
    for(size_t boneIdx(0); boneIdx<boneCount; boneIdx++)
    {
        instance.bones.push_back(NodeBinding());
        NodeBinding& bone = instance.bones.back();
        
        bone.sid = pSkin->GetJointIds()[boneIdx];
        bone.node = (Node*)pContInst->GetJoint(boneIdx)->GetUserHandle();
        assert(bone.node);
        //memcpy(&bone.invBindPose, &pSkin->GetBindPoses()[boneIdx], sizeof(bone.invBindPose));
        bone.invBindPose = pSkin->GetBindPoses()[boneIdx];
    }
    //memcpy(&instance.meshBindPose, &pSkin->GetBindShapeTransform(), sizeof(instance.meshBindPose));
    instance.meshBindPose = pSkin->GetBindShapeTransform();
    
    // Add bone idx and weight data to pMesh
    //---------------------------------------------------
    
    // Firt's let's try to use only the 4 most important influences
    // pSkin->ReduceInfluences(4);

    FCDWeightedMatches& vertexInfluences = pSkin->GetVertexInfluences();
    assert(vertexInfluences.size() == pMesh->oldIdxToNewIdxs.size());

    // Get the max # of influences to ensure we have an appropriate # of vtxAttribute arrays 
    //  allocated for this
    size_t maxInfCount(0);
    FCDWeightedMatches::iterator wmIt, wmEnd(vertexInfluences.end());
    for(wmIt = vertexInfluences.begin(); wmIt != wmEnd; ++wmIt)
        maxInfCount = max( maxInfCount, wmIt->size());
    
    // We will allocate ubyte4 and float4 arrays for bone-indices and weights respectively
    // so get the right number of these
    size_t numInfluenceArrays = (maxInfCount/4)+1;
    VtxAttributeArray::ScalarType indicesScalarType = (instance.bones.size() > 256) ? 
        VtxAttributeArray::ST_UINT32 : VtxAttributeArray::ST_UINT8;

    typedef std::vector<VtxAttributeArray*> VtxAttrArVector;
    VtxAttrArVector weightArrays;
    VtxAttrArVector indicesArrays;
    weightArrays.push_back(GetVtxAttributeArray(VtxAttributeArray::ST_FLOAT, 4,
        "BLENDWEIGHTS", VtxAttributeArray::BLENDWEIGHTS0, 0));
    indicesArrays.push_back(GetVtxAttributeArray(indicesScalarType, 4,
        "BLENDINDICES", VtxAttributeArray::BLENDINDICES0, 0));
    
    if(numInfluenceArrays>0)
    {
        // TODO: handle more than 8 weights (although it seems overkill)
        assert(numInfluenceArrays < 3);
        // for now let's cap at 8
        if( maxInfCount > 8)
        {
            pSkin->ReduceInfluences(8);
            maxInfCount = 8;
        }

        weightArrays.push_back(GetVtxAttributeArray(VtxAttributeArray::ST_FLOAT, 4,
            "BLENDWEIGHTS", VtxAttributeArray::BLENDWEIGHTS1, 1));
        indicesArrays.push_back(GetVtxAttributeArray(indicesScalarType, 4,
            "BLENDINDICES", VtxAttributeArray::BLENDINDICES1, 1));
    }

    MeshSubsetPtrList::iterator msIt(pMesh->subsets.begin()), msEnd(pMesh->subsets.end());
    for(; msIt != msEnd; ++msIt)
    {
        {
            VtxAttrArVector::iterator vtxAtIt(weightArrays.begin()), vtxAtEnd(weightArrays.end());
            for(;vtxAtIt != vtxAtEnd; ++vtxAtIt)
                (*msIt)->vtxAttributes.push_back(*vtxAtIt);
        }

        {
            VtxAttrArVector::iterator vtxAtIt(indicesArrays.begin()), vtxAtEnd(indicesArrays.end());
            for(;vtxAtIt != vtxAtEnd; ++vtxAtIt)
                (*msIt)->vtxAttributes.push_back(*vtxAtIt);
        }
    }

    // NOTE that at this point all the vertex BLENDWEIGHTn/BLENDINDICESn attribute arrays
    //    are sized to match the current number of vertices (m_vtxCount)
    //   Since we are not adding any vertices we will simply set the correct weight and bone/weight
    //    values below

    uint32 vtxIdx(0), weightIdx;
    for(wmIt = vertexInfluences.begin(); wmIt != wmEnd; ++wmIt, vtxIdx++)
    {
        // Each vertex in the original mesh maps to 1 or more verts in the current mesh 
        //   due to the conversion that it underwent in AddGeometry to contain a single index buffer
        FCDGeometryIndexTranslationMap::iterator oldVtoNewVIt = 
            pMesh->oldIdxToNewIdxs.find(vtxIdx);
        assert(oldVtoNewVIt != pMesh->oldIdxToNewIdxs.end());
        UInt32List& newVList = oldVtoNewVIt->second;

        UInt32List::iterator newVIt(newVList.begin()), newVEnd(newVList.end());
        for(; newVIt != newVEnd; ++newVIt)
        {
            uint32 vtxIdx = (*newVIt) + pMesh->vtxBegin;
            float weightSum(0);

            FCDJointWeightPairList::iterator jwpairIt(wmIt->begin()), jwpairEnd(wmIt->end());
            for(weightIdx=0; jwpairIt != jwpairEnd; ++jwpairIt, ++weightIdx)
            {
                weightSum += jwpairIt->weight;
                // "weightIdx / 4" because we store weights in groups of 4 in each array
                // "weightIdx & 0x3" to mask the lower bits and address the individual weight value
                (weightArrays[weightIdx / 4]->AsFloat(vtxIdx))[weightIdx & 0x3] = jwpairIt->weight;
                if( indicesScalarType == VtxAttributeArray::ST_UINT32)
                    (indicesArrays[weightIdx / 4]->AsUInt32(vtxIdx))[weightIdx & 0x3] = jwpairIt->jointIndex;
                else if( indicesScalarType == VtxAttributeArray::ST_UINT8)
                    (indicesArrays[weightIdx / 4]->AsUInt8(vtxIdx))[weightIdx & 0x3] = uint8(jwpairIt->jointIndex);
            }

            // make sure weights are normalized and add up to one
            assert(abs(weightSum-1.0f) < 0.001f );
            
            // fill the remaining weights with invalid values
            for(;weightIdx < maxInfCount; weightIdx++ )
            {
                (weightArrays[weightIdx / 4]->AsFloat(vtxIdx))[weightIdx & 0x3] = 0;
                if( indicesScalarType == VtxAttributeArray::ST_UINT32)
                    (indicesArrays[weightIdx / 4]->AsUInt32(vtxIdx))[weightIdx & 0x3] = uint32(-1);
                else if( indicesScalarType == VtxAttributeArray::ST_UINT8)
                    (indicesArrays[weightIdx / 4]->AsUInt8(vtxIdx))[weightIdx & 0x3] = uint8(-1);

            }
        }
    }
    
    pMesh->hasSkin = true;

    return true;
}

SkinnedMeshImpl::Mesh * SkinnedMeshImpl::GetGeometry(FCDEntity *pEntity)
{
    Mesh *pMesh;
    // check it was added already and if so, add an instance refering to it
    IdToMeshMap::iterator meshIt = m_idToMesh.find(pEntity->GetDaeId());
    if(meshIt != m_idToMesh.end())
    {
        pMesh = meshIt->second;
        assert(pMesh);
    }
    else
    {
        pMesh = AddGeometry(pEntity);
    }

    return pMesh;
}

SkinnedMeshImpl::Mesh * SkinnedMeshImpl::AddGeometry(FCDEntity *pEntity)
{
    FCDGeometry *pGeom = dynamic_cast<FCDGeometry *>(pEntity);

    // We are only interested in meshes
    if(!pGeom->IsMesh())
        return NULL;

    FCDGeometryMesh *pGeomMesh = pGeom->GetMesh();
    if(!pGeomMesh)
        // The geometry is undefined
        return NULL;


    // Add mesh
    m_meshes.push_back(Mesh());
    Mesh *pMesh = &(m_meshes.back());
    pMesh->id = pGeomMesh->GetDaeId();
    pMesh->hasSkin = false;
    m_idToMesh.insert(IdToMeshMap::value_type(pMesh->id, pMesh));

    // Make sure it is a triangle mesh, otherwise triangulate
    if(!pGeomMesh->IsTriangles())
        FCDGeometryPolygonsTools::Triangulate(pGeomMesh);

    // Prepare to have a single index buffer    
    FCDGeometryPolygonsTools::GenerateUniqueIndices(pGeomMesh, NULL, &pMesh->oldIdxToNewIdxs);

    // Process mesh triangle sets, inserting (if new) or getting vertex arrays as needed
    //   In this way we only add vertex arrays for those input sources that are actually used
    for(size_t polyIdx(0); polyIdx<pGeomMesh->GetPolygonsCount(); polyIdx++)
    {
        FCDGeometryPolygons *pPolys = pGeomMesh->GetPolygons(polyIdx);

        FCDGeometrySource* pPosSource = pGeomMesh->GetPositionSource();
        assert(pPosSource);
        if(!pPosSource)
            // we don't want any mesh with no POSITION attributes, 
            // FIXME: if this happens the index and vtxAttribute arrays may be out of sync
            continue;

        MeshSubset *pMeshSubset(NULL);

        // Append index buffer adding appropriate offset to each index
        //------------------------------------------------------------
        size_t vtxBaseOffset = m_vtxCount;
        UInt32List *pIndexList = pPolys->FindIndices(pPosSource);
        assert(pIndexList);
        if(pIndexList)
        {
            // Create a new MeshSubset
            m_meshSubsets.push_back(MeshSubset());
            pMeshSubset = &(m_meshSubsets.back());
            pMeshSubset->idxBegin = static_cast<uint32>(m_indices.size());
            pMeshSubset->idxCount = static_cast<uint32>(pIndexList->size());
            pMeshSubset->materialSemantic = pPolys->GetMaterialSemantic();
            // append it to this mesh's list of subsets                    
            pMesh->subsets.push_back(pMeshSubset);

            // insert the indices into the global index list
            assert((pPolys->GetFaceCount()*3)== pIndexList->size());
            m_indices.reserve(m_indices.size() + pIndexList->size());
            UInt32List::iterator idxIt(pIndexList->begin()), idxEnd(pIndexList->end());
            for(; idxIt != idxEnd; ++idxIt)
                m_indices.push_back(static_cast<uint32>(vtxBaseOffset+(*idxIt)));
        }
        m_triCount = static_cast<uint32>(m_indices.size());

        assert(pMeshSubset);
        pMeshSubset->vtxAttributes.reserve(pPolys->GetInputCount());

        // Traverse inputs binding inputSources to appropriate vtxAttribute arrays
        //-------------------------------------------------------------------------
        //  (or adding a new vtxAttribute array if needed)
        
        for(size_t inputIdx(0); inputIdx<pPolys->GetInputCount(); inputIdx++)
        {
            FCDGeometryPolygonsInput *pInput = pPolys->GetInput(inputIdx);
            FCDGeometrySource* pInputSource = pInput->GetSource();

            FUDaeGeometryInput::Semantic fsemantic = pInputSource->GetType();
            FSemanticToSemanticMap::iterator semanticIt = m_fsemToSemMap.find(fsemantic);
            
            // We should have a complete enum mapping in m_fsemToSemMap
            assert(semanticIt != m_fsemToSemMap.end());
            if(semanticIt == m_fsemToSemMap.end())
                // but new FCollada dlls might contain other semantic enums we don't support
                continue; 

            int32 tcSet = pInput->GetSet();
            if( tcSet < 0 ) tcSet = 0;
            
            VtxAttributeArray *dstVtxArray =  GetVtxAttributeArray( VtxAttributeArray::ST_FLOAT,
                pInputSource->GetStride(), FUDaeGeometryInput::ToString(fsemantic), semanticIt->second, tcSet );
            assert(dstVtxArray);

            // we store the pointer to the destination vertex array in pInputSource 
            //  so we can use it in a second pass over all input sources
            assert((pInputSource->GetUserHandle() == NULL) ||
                    (pInputSource->GetUserHandle() == dstVtxArray));
            pInputSource->SetUserHandle(dstVtxArray);

            pMeshSubset->vtxAttributes.push_back(dstVtxArray);
        }
    }


    // Append inputsources to arrays of vertex attributes
    //---------------------------------------------------
    int commonElemCount(-1);
    // (they should all have the same # of elements)
    pMesh->vtxAttributes.reserve(pGeomMesh->GetSourceCount());
    for(size_t srcIdx(0); srcIdx<pGeomMesh->GetSourceCount(); srcIdx++)
    {
        FCDGeometrySource* pSource = pGeomMesh->GetSource(srcIdx);
        size_t elemCount = pSource->GetDataCount() / pSource->GetStride();

        if(commonElemCount < 0)
            commonElemCount = static_cast<int>(elemCount);
        assert(commonElemCount == elemCount);

        // we should have saved earlier a pointer to the appropriate VtxAttributeArray
        VtxAttributeArray *dstVtxArray = 
            static_cast<VtxAttributeArray *>(pSource->GetUserHandle());
        assert(dstVtxArray);

        dstVtxArray->AppendData(pSource->GetStride(), 
            static_cast<int>(pSource->GetDataCount()), &pSource->GetData()[0]);       

        pMesh->vtxAttributes.push_back(dstVtxArray);

        if(pSource->GetType() == FUDaeGeometryInput::POSITION)
        {
            // compute the bounding sphere of this mesh
            FMVector3 minPos(FLT_MAX, FLT_MAX, FLT_MAX);
            FMVector3 maxPos(FLT_MIN, FLT_MIN, FLT_MIN);
            for(uint32 posIdx(0); posIdx<dstVtxArray->m_count; posIdx++)
            {
                FMVector3 v(dstVtxArray->AsFloat(posIdx));
                minPos.x = min(minPos.x, v.x);
                minPos.y = min(minPos.y, v.y);
                minPos.z = min(minPos.z, v.z);
                
                maxPos.x = max(maxPos.x, v.x);
                maxPos.y = max(maxPos.y, v.y);
                maxPos.z = max(maxPos.z, v.z);
            }
            
            pMesh->sphereCenter = 0.5f * (minPos + maxPos);
            pMesh->sphereRadius = 0.5f * (maxPos - minPos).Length();
        }
    }
    

    // Save vertex range and increase the vertex count
    //---------------------------------------------------
    pMesh->vtxBegin = static_cast<uint32>(m_vtxCount);
    pMesh->vtxCount = static_cast<uint32>(commonElemCount);
    m_vtxCount += commonElemCount;


    // Pad with zeroes arrays that we didn't append into
    //---------------------------------------------------
    int diffElemCount(-1);
    VtxAttributeArrayList::iterator vtxAttrIt(m_vtxAttributes.begin()), vtxAttrEnd(m_vtxAttributes.end());
    for(; vtxAttrIt!=vtxAttrEnd; ++vtxAttrIt)
    {
        if(vtxAttrIt->m_count < m_vtxCount)
        {
            assert(vtxAttrIt->m_count <= m_vtxCount);
            if(diffElemCount < 0)
                diffElemCount = m_vtxCount - vtxAttrIt->m_count;
            assert(diffElemCount == (m_vtxCount - vtxAttrIt->m_count));
            vtxAttrIt->AppendData(vtxAttrIt->m_info.m_compCount, vtxAttrIt->m_info.m_compCount*diffElemCount, 0);
        }
    }

    return pMesh;
}

SkinnedMeshImpl::VtxAttributeArray * SkinnedMeshImpl::GetVtxAttributeArray(
    VtxAttributeArray::ScalarType type, uint32 compCount, const std::string &strSemantic,
    VtxAttributeArray::Semantic semantic, uint32 semanticIdx)
{
    VtxAttributeArray *retVtxArray(NULL);

    VtxAttributeArray::VtxAttributeArrayInfo vaInfo( semantic, semanticIdx, type, compCount);
    SemanticToVtxAttrArMap::iterator vtxAttrArIt = m_semToVtxAttrAr.find(vaInfo);
    if(vtxAttrArIt != m_semToVtxAttrAr.end())
    {
        retVtxArray = vtxAttrArIt->second;
    }
    else
    {
        // insert a new vertex array for this semantic
        m_vtxAttributes.push_back(
            VtxAttributeArray(type, compCount, strSemantic, semantic, semanticIdx));
        retVtxArray = &(m_vtxAttributes.back());
        m_semToVtxAttrAr[vaInfo] = retVtxArray;

        // pad this new vtxAttribute array with 0s
        retVtxArray->AppendData(compCount, m_vtxCount*compCount, 0);
    }

    return retVtxArray;
}

void SkinnedMeshImpl::GenSubsetToMaterialMap(MsPtrToMatPtrMap &dstSubsetToMaterialMap,
    const MeshSubsetPtrList &subsetPtrs, FCDEntityInstance *pGeometryInstance)
{
    FCDGeometryInstance* pGeomInst = dynamic_cast<FCDGeometryInstance*>(pGeometryInstance);
    assert(pGeomInst);
    
    dstSubsetToMaterialMap.clear();
    
    MeshSubsetPtrList::const_iterator msIt(subsetPtrs.begin()), msEnd(subsetPtrs.end());
    for(; msIt != msEnd; ++msIt)
    {
        FCDMaterialInstance* pMatInst = 
            pGeomInst->FindMaterialInstance((*msIt)->materialSemantic);

        Material *pMat = GetMaterial(pMatInst);
        if(pMat)
            dstSubsetToMaterialMap[*msIt] = pMat;
    }
}


SkinnedMeshImpl::Material * SkinnedMeshImpl::GetMaterial(FCDMaterialInstance *pMatInst)
{
    Material *pMat(NULL);
    IdToMaterialMap::iterator matIt = m_idToMaterial.find(pMatInst->GetEntity()->GetDaeId());
    if(matIt != m_idToMaterial.end())
    {
        pMat = matIt->second;
        assert(pMat);
    }
    else
    {
        pMat = AddMaterial(pMatInst);
    }
    return pMat;
}

SkinnedMeshImpl::Material * SkinnedMeshImpl::AddMaterial(FCDMaterialInstance *pMatInst)
{
    FCDMaterial *pMatEnt = pMatInst->FlattenMaterial();
    Material *pMat(NULL);
    
    if(!pMatEnt)
        goto AddMaterial_cleanup;

    // Get the effect
    FCDEffect *pEff = pMatEnt->GetEffect();
    if(!pEff)
        goto AddMaterial_cleanup;
    
    // Get a profile that we can process
    FCDEffectProfile *pEffProf = NULL;
    size_t pEffProfileCount = pEff->GetProfileCount();
    for( size_t pIdx=0; pIdx<pEffProfileCount; pIdx++)
    {
        // TODO: support other profiles (currently we only support the COMMON profile)
        FCDEffectProfile *pEP = pEff->GetProfile(pIdx);
        assert(pEP);
        if( pEP->GetType() == FUDaeProfileType::COMMON )
        {
            pEffProf = (pEP);
            break;
        }
    }

    if(!pEffProf)
        goto AddMaterial_cleanup;
    
    FCDEffectStandard *pEffStd = dynamic_cast<FCDEffectStandard *>(pEffProf);
    if(!pEffStd)
        goto AddMaterial_cleanup;

    m_materials.push_back(Material());
    pMat = &m_materials.back();

    pMat->id = pMatInst->GetEntity()->GetDaeId();

    // TODO: support others channels (currently we only support DIFFUSE, SPECULAR and NORMAL)
    uint32 supportChannelsIdMap[] = {  FUDaeTextureChannel::DIFFUSE,  BoundTexture::DIFFUSE,
                                       FUDaeTextureChannel::SPECULAR, BoundTexture::SPECULAR,
                                       FUDaeTextureChannel::BUMP,     BoundTexture::NORMAL,
                                     };
    size_t supChannelIdCnt = sizeof(supportChannelsIdMap) / (2*sizeof(uint32));

    for(size_t i=0; i<supChannelIdCnt; i++)
    {
        uint32 channelId = supportChannelsIdMap[i*2];
        uint32 texSemantic = supportChannelsIdMap[(i*2) + 1];

        size_t texCount = pEffStd->GetTextureCount(supportChannelsIdMap[i*2]);
        if(texCount)
        {
            // TODO: support multiple textures per channel
            FCDTexture *pTex = pEffStd->GetTexture(channelId, 0);
            assert(pTex);
            FCDImage *pImg = pTex->GetImage();
            if( pImg)
            {
                Texture *pTex = GetTexture(pImg);
                if(pTex)
                    pMat->boundTextures.push_back(BoundTexture(pTex, texSemantic));
            }
        }
    }


    if(pMat->boundTextures.size())
    {
        m_idToMaterial[pMat->id] = pMat;
    }
    else
    {
        m_materials.pop_back();
        pMat = NULL;
    }

AddMaterial_cleanup:
    SAFE_DELETE(pMatEnt);
    return pMat;
}

SkinnedMeshImpl::Texture * SkinnedMeshImpl::GetTexture(FCDImage *pImg)
{
    Texture *pTex(NULL);
    IdToTextureMap::iterator texIt = m_idToTexture.find(pImg->GetDaeId());
    if(texIt != m_idToTexture.end())
    {
        pTex = texIt->second;
        assert(pTex);
    }
    else
    {
        pTex = AddTexture(pImg);
    }
    return pTex;
}

SkinnedMeshImpl::BoundTexture::BoundTexture(Texture *pTex, uint32 sem)
{
    pTexture = pTex;
    semantic = (Semantic)sem;
    if( semantic >= COUNT )
        semantic = UNKNOWN;
}


SkinnedMeshImpl::Texture * SkinnedMeshImpl::AddTexture(FCDImage *pImg)
{
    m_textures.push_back(Texture());
    Texture *pTex = &m_textures.back();

    pTex->id = pImg->GetDaeId();
    pTex->fileName = pImg->GetFilename();

    m_idToTexture[pTex->id] = pTex;

    return pTex;
}

// END Loading Code
//////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////
// BEGIN Animation Code

void SkinnedMeshImpl::Update(float elapsedTime)
{
    float newTime(m_currentTime);
    // Handle time advance based on animation playback options
    if((m_currentTime+elapsedTime) > m_endTime)
    {
        float endMinusStart = m_endTime-m_startTime;
        if( endMinusStart > 0.001)
        {
            switch(m_animEndHandling)
            {
            case LOOP:
                {
                float overTime = (m_currentTime + elapsedTime - m_endTime);
                newTime = m_startTime + fmod(overTime, endMinusStart);
                }
                break;
            case BLOCK:
            default:
                newTime = m_endTime;
                break;
            }
        }
        else
        {
            newTime = m_currentTime;
        }
    }
    else
    {
        newTime = m_currentTime + elapsedTime;  
    };
    
    SetCurrentTime(newTime);
}

void SkinnedMeshImpl::SetStartEndTimes(float startTime, float endTime)
{
    // The only thing we enforce is endTime being >= to startTime
    // This would change if we allow playing backwards
    if(startTime > endTime)
        endTime = startTime;

    m_startTime = startTime;
    m_endTime = endTime;
}

void SkinnedMeshImpl::GetStartEndTimes(float *outStartTime, float *outEndTime)
{
    if(outStartTime)
        *outStartTime = m_startTime;
    if(outEndTime)
        *outEndTime = m_endTime;
}

void SkinnedMeshImpl::SetCurrentTime(float currentTime)
{
    m_currentTime = currentTime;
    assert(m_minTime <= m_maxTime);

    if(m_currentTime > m_maxTime)
        UpdateNodes(m_maxTime);
    else if(m_currentTime < m_minTime)
        UpdateNodes(m_minTime);
    else
        UpdateNodes(m_currentTime);
}

void SkinnedMeshImpl::SetAnimationEndHandling(ANIMATION_END_HANDLING aeh)
{
    m_animEndHandling = aeh;
}

void SkinnedMeshImpl::UpdateNodes(float timeValue)
{
    assert((timeValue>=m_minTime) && (timeValue <=m_maxTime));

    if(m_lastNodesUpdateTime == timeValue)
        return;


    NodeList::iterator nodeIt(m_nodes.begin()), nodeEnd(m_nodes.end());
    for( ; nodeIt != nodeEnd; ++nodeIt)
    {
        if(nodeIt->pParent == NULL)
            UpdateNode(&(*nodeIt), timeValue);
    }

    m_lastNodesUpdateTime = timeValue;
}

void SkinnedMeshImpl::UpdateNode(Node *pNode, float timeValue)
{
    // update the local xform for the current time
    pNode->EvaluateLocalXForm(timeValue);

    // update the world xform prepending the parent's
    if(pNode->pParent)
        pNode->worldXForm = pNode->pParent->worldXForm * pNode->xform;
    else
        pNode->worldXForm = pNode->xform;

    // update the children
    NodePtrList::iterator childIt(pNode->children.begin()), childEnd(pNode->children.end());
    for( ; childIt != childEnd; ++childIt)
    {
        UpdateNode(*childIt, timeValue);
    }
}

// END Animation Code
//////////////////////////////////////////////////////////////////////////////


//----------------------------------------------------------------------------------
// Class VtxAttributeArray::VtxAttributeArrayInfoLess
//----------------------------------------------------------------------------------

bool SkinnedMeshImpl::VtxAttributeArray::VtxAttributeArrayInfoLess::operator ()
    (const VtxAttributeArrayInfo &pLeft, const VtxAttributeArrayInfo &pRight) const
{
    if( pLeft.m_semantic == pRight.m_semantic )
    {
        if( pLeft.m_semIdx == pRight.m_semIdx )
        {
            if( pLeft.m_type == pRight.m_type )
            {
                return pLeft.m_compCount < pRight.m_compCount;
            }
            return pLeft.m_type < pRight.m_type;
        }
        return pLeft.m_semIdx < pRight.m_semIdx;
    }
    return pLeft.m_semantic < pRight.m_semantic;
}

SkinnedMeshImpl::VtxAttributeArray::VtxAttributeArrayInfo::VtxAttributeArrayInfo(
    Semantic semantic, 
    uint32 semanticIdx,
    ScalarType type,
    uint32 compCount ) : m_semantic(semantic), m_semIdx(semanticIdx),
                         m_type(type), m_scalarSize(0), m_compCount(compCount)
{
    // We don't want vertex attribute arrays larger than 4 components due to hw constraints
    // although we could split them into multiple vertex attribute arrays
    assert(compCount<=4);

    switch(m_type)
    {
        case ST_FLOAT:
            m_scalarSize = sizeof(float);
            break;
        case ST_SINT32:
        case ST_SNORM32:
            m_scalarSize = sizeof(int32);
            break;
        case ST_UINT32:
        case ST_UNORM32:
            m_scalarSize = sizeof(uint32);
            break;
        case ST_SINT8:
        case ST_SNORM8:
            m_scalarSize = sizeof(int8);
            break;
        case ST_UINT8:
        case ST_UNORM8:
            m_scalarSize = sizeof(uint8);
            break;
        default:
            assert(!"VtxAttributeArray: Invalid ScalarType");
    }
}


//----------------------------------------------------------------------------------
// Class VtxAttributeArray
//----------------------------------------------------------------------------------

SkinnedMeshImpl::VtxAttributeArray::VtxAttributeArray( ScalarType type, uint32 compCount, 
            const std::string &strSemantic, Semantic semantic, uint32 semanticIdx)
    : m_strSemantic(strSemantic), m_info(semantic, semanticIdx, type, compCount), 
      m_count(0), m_pData(NULL)
{
}

bool SkinnedMeshImpl::VtxAttributeArray::AppendData(uint32 compCount, uint32 scalarCount, void *srcData)
{
    assert(m_info.m_compCount && m_info.m_scalarSize);
    assert(compCount == m_info.m_compCount);

    if(compCount != m_info.m_compCount)
        return false;
    
    if(scalarCount == 0)
        return true;
   
    int roundedUpScalarCount = scalarCount;
    int mod = (scalarCount % m_info.m_compCount);
    if(mod != 0)
        roundedUpScalarCount += m_info.m_compCount - mod;

    int prevByteCount = m_count * m_info.m_compCount * m_info.m_scalarSize;
    int appendedByteCount = roundedUpScalarCount * m_info.m_scalarSize;
    int newByteCount = prevByteCount + appendedByteCount;
    
    char *data(NULL);
    assert(sizeof(char)==1);
    // allocate data
    try {
        data = new char[newByteCount];
    }
    catch(...) {
        return false;
    }
    assert(data);

    if(m_pData)
    {
        assert(prevByteCount);
        memcpy(data, m_pData, prevByteCount);
        delete [] m_pData;
    }
    if(srcData)
    {
        memcpy(data+prevByteCount, srcData, appendedByteCount);
    }
    else
    {
        // pad with 0s
        memset(data+prevByteCount, 0, appendedByteCount);
    }

    m_pData = data;
    m_count += (roundedUpScalarCount / m_info.m_compCount);

    return true;
}

SkinnedMeshImpl::VtxAttributeArray::~VtxAttributeArray()
{
    delete [] m_pData;
    m_pData = NULL;
}


//----------------------------------------------------------------------------------
// Class Node
//----------------------------------------------------------------------------------
void SkinnedMeshImpl::Node::EvaluateLocalXForm(float timeValue)
{
    if(xformKeyFrames.size() == 0)
        return;

    //find keyframe before and after and do linear interpolation between them
    FloatToMatrix44Map::iterator prevKey(xformKeyFrames.lower_bound(timeValue));
    FloatToMatrix44Map::iterator nextKey(prevKey);
   
    if( (prevKey == xformKeyFrames.end()) ||
        ((prevKey != xformKeyFrames.begin()) && (prevKey->first > timeValue)) )
        --prevKey;

    // because xformKeyFrames.size() != 0 prevKey should be a valid iterator here
    assert(prevKey != xformKeyFrames.end());

    if( (prevKey == nextKey) || (nextKey == xformKeyFrames.end()))
    {
        xform = prevKey->second;
        return;
    }


    float time0 = prevKey->first;
    float time1 = nextKey->first;
    float s = (timeValue - time0)/(time1-time0);

    const FMMatrix44& m0 = prevKey->second;
    const FMMatrix44& m1 = nextKey->second;


    FMMatrix44 mNew = m0;

    // Get axis vectors
    FMVector3 m0x( m0[0] ), m0y( m0[1] ), m0z( m0[2] );
    FMVector3 m1x( m1[0] ), m1y( m1[1] ), m1z( m0[2] );

    if( (m0x.LengthSquared() > 1.01) || (m0y.LengthSquared() > 1.01) || (m0z.LengthSquared() > 1.01) ||
        (m1x.LengthSquared() > 1.01) || (m1y.LengthSquared() > 1.01) || (m1z.LengthSquared() > 1.01) )
    {
        // TODO: need to interpolate scaling too
    }
    else
    {
        // Get translation components
        FMVector3 t0 = m0.GetTranslation();
        FMVector3 t1 = m1.GetTranslation();

        // Get axis of rotation from m0 to m1
        FMVector3 dX(m1x - m0x), dY(m1y - m0y), dZ(m1z - m0z);
        FMVector3 axis = (dX ^ dY) + (dY ^ dZ) + (dZ ^ dX);

        #define VLENGTH_EPSILON 0.0001f
        if( axis.Length() < VLENGTH_EPSILON )
        {
            mNew = m0;
        }
        else
        {
            FMVector3 dU = dX;
            FMVector3 U = m0x;
            if( dU.Length() < VLENGTH_EPSILON )
            {
                dU = dY;
                U = m0y;
            }
            assert(dU.Length() >= VLENGTH_EPSILON);
            axis.NormalizeIt();
            // Get angle of rotation from m0 to m1
            float angle = 2.0f * asin( dU.Length() / (2.0f * (axis ^ U).Length()) );
            // Interpolate angle
            float aNew = s * angle;
            mNew.AxisRotationMatrix(axis, aNew);
        }

        // Interpolate translation
        FMVector3 tNew = t0 + s * (t1 - t0);
        mNew.SetTranslation(tNew);
    }

    xform = mNew;
}



//////////////////////////////////////////////////////////////////////////////
// BEGIN DX10 Rendering Code

//----------------------------------------------------------------------------------
// Class SkinnedMeshD3D10
//----------------------------------------------------------------------------------

SkinnedMeshD3D10::SkinnedMeshD3D10() : m_pd3dDevice(NULL), m_pIndexBuffer(NULL),
    m_pSOInputLayout(NULL), m_pTmpSOInputLayout(NULL), m_pEffect(NULL),
    m_pSkinnedTechnique(NULL), m_pRigidTechnique(NULL), m_pSkinnedSOTechnique(NULL),
    m_pRigidSOTechnique(NULL), m_pFromSOTechnique(NULL), m_pWorldVar(NULL),
    m_pViewProjectionVar(NULL), m_pBonesVar(NULL),
    m_pEyeVar(NULL), m_pDiffuseTexVar(NULL), m_pSpecularTexVar(NULL), m_pNormalTexVar(NULL), 
    m_pUserNormalMapVar(NULL), m_IsRenderingToSO(false), m_IsRenderingFromSO(false),
    m_dstSoBuffer(0), m_srcSoBuffer(0), m_pIncludeNodes(NULL), m_pExcludeNodes(NULL)
{

}

SkinnedMeshD3D10::~SkinnedMeshD3D10()
{
    // release index buffer
    SAFE_RELEASE(m_pIndexBuffer);

    // release vertex buffers
    ID3D10BufferVector::iterator vtxBufIt(m_vtxBuffers.begin()), vtxBufEnd(m_vtxBuffers.end());
    for(; vtxBufIt != vtxBufEnd; ++vtxBufIt)
        SAFE_RELEASE(*vtxBufIt);

    // release effect
    SAFE_RELEASE(m_pEffect);

    // release input layouts
    ID3D10InputLayoutVector::iterator inLayoutIt(m_inputLayouts.begin()), inLayoutEnd(m_inputLayouts.end());
    for(; inLayoutIt != inLayoutEnd; ++inLayoutIt)
        SAFE_RELEASE(*inLayoutIt);

    // release stremout buffers
    ID3D10BufferVector::iterator soBufIt(m_SOBuffers.begin()), soBufEnd(m_SOBuffers.end());
    for(; soBufIt != soBufEnd; ++soBufIt)
        SAFE_RELEASE(*soBufIt);

    // release input layout for streamout buffers
    SAFE_RELEASE(m_pSOInputLayout);

    // release textures
    ID3D10SRVVector::iterator texSRVIt(m_textureSRVs.begin()), texSRVEnd(m_textureSRVs.end());
    for(; texSRVIt != texSRVEnd; ++texSRVIt)
    {
        SAFE_RELEASE(*texSRVIt);
    }
    
    
    // release device
    SAFE_RELEASE(m_pd3dDevice);
}

bool SkinnedMeshD3D10::VtxAttrSemanticLess(VtxAttributeArray *pLeft, VtxAttributeArray *pRight)
{
    if( pLeft->m_info.m_semantic == pRight->m_info.m_semantic )
        return pLeft->m_info.m_semIdx < pRight->m_info.m_semIdx;
    
    return pLeft->m_info.m_semantic < pRight->m_info.m_semantic;
}

DXGI_FORMAT SkinnedMeshD3D10::DXGIFormatFromVtxAttr(VtxAttributeArray *pVtxAtAr)
{
    DXGI_FORMAT retFormat(DXGI_FORMAT_UNKNOWN);

    assert((pVtxAtAr->m_info.m_compCount>0) && (pVtxAtAr->m_info.m_compCount <= 4));
    assert(pVtxAtAr->m_info.m_type < VtxAttributeArray::SCALAR_TYPE_COUNT);
    
    switch(pVtxAtAr->m_info.m_type)
    {
    case VtxAttributeArray::ST_FLOAT:
        switch(pVtxAtAr->m_info.m_compCount)
        {
        case 1: retFormat = DXGI_FORMAT_R32_FLOAT; break;
        case 2: retFormat = DXGI_FORMAT_R32G32_FLOAT; break;
        case 3: retFormat = DXGI_FORMAT_R32G32B32_FLOAT; break;
        case 4: retFormat = DXGI_FORMAT_R32G32B32A32_FLOAT; break;
        }
        break;
    case VtxAttributeArray::ST_SINT32:
        switch(pVtxAtAr->m_info.m_compCount)
        {
        case 1: retFormat = DXGI_FORMAT_R32_SINT; break;
        case 2: retFormat = DXGI_FORMAT_R32G32_SINT; break;
        case 3: retFormat = DXGI_FORMAT_R32G32B32_SINT; break;
        case 4: retFormat = DXGI_FORMAT_R32G32B32A32_SINT; break;
        }
        break;
    case VtxAttributeArray::ST_UINT32:
        switch(pVtxAtAr->m_info.m_compCount)
        {
        case 1: retFormat = DXGI_FORMAT_R32_UINT; break;
        case 2: retFormat = DXGI_FORMAT_R32G32_UINT; break;
        case 3: retFormat = DXGI_FORMAT_R32G32B32_UINT; break;
        case 4: retFormat = DXGI_FORMAT_R32G32B32A32_UINT; break;
        }
        break;
    case VtxAttributeArray::ST_SNORM32:
        // not supported
        break;
    case VtxAttributeArray::ST_UNORM32:
        // not supported
        break;
    case VtxAttributeArray::ST_SINT8:
        switch(pVtxAtAr->m_info.m_compCount)
        {
        case 1: retFormat = DXGI_FORMAT_R8_SINT; break;
        case 2: retFormat = DXGI_FORMAT_R8G8_SINT; break;
        case 3: break; // not supported
        case 4: retFormat = DXGI_FORMAT_R8G8B8A8_SINT; break;
        }
        break;
    case VtxAttributeArray::ST_UINT8:
        switch(pVtxAtAr->m_info.m_compCount)
        {
        case 1: retFormat = DXGI_FORMAT_R8_UINT; break;
        case 2: retFormat = DXGI_FORMAT_R8G8_UINT; break;
        case 3: break; // not supported
        case 4: retFormat = DXGI_FORMAT_R8G8B8A8_UINT; break;
        }
        break;
    case VtxAttributeArray::ST_SNORM8:
        switch(pVtxAtAr->m_info.m_compCount)
        {
        case 1: retFormat = DXGI_FORMAT_R8_SNORM; break;
        case 2: retFormat = DXGI_FORMAT_R8G8_SNORM; break;
        case 3: break; // not supported
        case 4: retFormat = DXGI_FORMAT_R8G8B8A8_SNORM; break;
        }
        break;
    case VtxAttributeArray::ST_UNORM8:
    default:
        switch(pVtxAtAr->m_info.m_compCount)
        {
        case 1: retFormat = DXGI_FORMAT_R8_UNORM; break;
        case 2: retFormat = DXGI_FORMAT_R8G8_UNORM; break;
        case 3: break; // not supported
        case 4: retFormat = DXGI_FORMAT_R8G8B8A8_UNORM; break;
        }
        break;
    }
    return retFormat;
}

#define VRETFALSE( expr ) V( expr ); if(FAILED(hr)) return false;
#define GET_TECHNIQUE(pDstT, pEff, str) { pDstT = pEff->GetTechniqueByName(str); assert(pDstT->IsValid()); }

bool SkinnedMeshD3D10::InitializeForRendering(ID3D10Device* pd3dDevice, int maxSoBufNum)
{
    HRESULT hr;

    SAFE_ACQUIRE(m_pd3dDevice, pd3dDevice);

    // create index buffer
    {
        D3D10_BUFFER_DESC bufferDesc;
        bufferDesc.Usage           = D3D10_USAGE_IMMUTABLE;
        bufferDesc.ByteWidth       = static_cast<UINT>(sizeof(m_indices[0]) * m_indices.size());
        bufferDesc.BindFlags       = D3D10_BIND_INDEX_BUFFER;
        bufferDesc.CPUAccessFlags  = 0;
        bufferDesc.MiscFlags       = 0;

        D3D10_SUBRESOURCE_DATA initData;
        initData.pSysMem = &m_indices[0];
        initData.SysMemPitch = 0;
        initData.SysMemSlicePitch = 0;
        VRETFALSE(m_pd3dDevice->CreateBuffer(&bufferDesc, &initData, &m_pIndexBuffer));
    }      

    // create vertex buffers
    {
        m_vtxBuffers.reserve( m_vtxAttributes.size());

        VtxAttributeArrayList::iterator vtxAtIt(m_vtxAttributes.begin()), vtxAtEnd(m_vtxAttributes.end());
        for(; vtxAtIt != vtxAtEnd; ++vtxAtIt)
        {
            ID3D10Buffer *pNewVtxBuffer(NULL);

            D3D10_BUFFER_DESC bufferDesc;
            bufferDesc.Usage           = D3D10_USAGE_IMMUTABLE;
            bufferDesc.ByteWidth       = vtxAtIt->m_info.m_scalarSize * vtxAtIt->m_info.m_compCount * vtxAtIt->m_count;
            bufferDesc.BindFlags       = D3D10_BIND_VERTEX_BUFFER;
            bufferDesc.CPUAccessFlags  = 0;
            bufferDesc.MiscFlags       = 0;

            D3D10_SUBRESOURCE_DATA initData;
            initData.pSysMem = vtxAtIt->AsFloat();
            initData.SysMemPitch = 0;
            initData.SysMemSlicePitch = 0;
            VRETFALSE(m_pd3dDevice->CreateBuffer(&bufferDesc, &initData, &pNewVtxBuffer));

            m_vtxBuffers.push_back(pNewVtxBuffer);
            m_vtxAtrToVtxBuffers[&(*vtxAtIt)] = pNewVtxBuffer;
        }
    }

    // load effect file
    {
        DWORD dwShaderFlags = D3D10_SHADER_ENABLE_STRICTNESS;
        WCHAR fullPath[MAX_PATH];
        VRETFALSE(NVUTFindDXSDKMediaFileCch( fullPath, MAX_PATH, L"SkinnedMesh.fx" ));
        VRETFALSE(D3DX10CreateEffectFromFile(fullPath, NULL, NULL, dwShaderFlags,
            0, pd3dDevice, NULL, NULL, &m_pEffect, NULL ));

        GET_TECHNIQUE(m_pSkinnedTechnique   , m_pEffect, "SkinnedMesh" );
        GET_TECHNIQUE(m_pRigidTechnique     , m_pEffect, "RigidMesh");
        GET_TECHNIQUE(m_pSkinnedSOTechnique , m_pEffect, "SkinnedMeshSO");
        GET_TECHNIQUE(m_pRigidSOTechnique   , m_pEffect, "RigidMeshSO");

        m_pWorldVar                 = m_pEffect->GetVariableByName("WorldMatrix")->AsMatrix();
        m_pViewProjectionVar        = m_pEffect->GetVariableByName("ViewProjectionMatrix")->AsMatrix();
        m_pBonesVar                 = m_pEffect->GetVariableByName("Bones")->AsMatrix();
        m_pEyeVar                   = m_pEffect->GetVariableByName("EyePos")->AsVector();
        m_pDiffuseTexVar            = m_pEffect->GetVariableByName("sceneTextureDiffuse")->AsShaderResource();
        m_pSpecularTexVar           = m_pEffect->GetVariableByName("sceneTextureSpecular")->AsShaderResource();
        m_pNormalTexVar             = m_pEffect->GetVariableByName("sceneTextureNormal")->AsShaderResource();
        m_pUserNormalMapVar         = m_pEffect->GetVariableByName("useNormalMap")->AsScalar();
        assert(m_pWorldVar->IsValid());
        assert(m_pViewProjectionVar->IsValid());
        assert(m_pBonesVar->IsValid());
        assert(m_pEyeVar->IsValid());
        assert(m_pDiffuseTexVar->IsValid());
        assert(m_pSpecularTexVar->IsValid());
        assert(m_pNormalTexVar->IsValid());
        assert(m_pUserNormalMapVar->IsValid());

    }
    
    // create vertex layouts for all meshSubsets
    {
        typedef std::vector<D3D10_INPUT_ELEMENT_DESC> InputElemDescVector;
        MeshSubsetList::iterator msIt(m_meshSubsets.begin()), msEnd(m_meshSubsets.end());
        for(; msIt != msEnd; ++msIt)
        {
            // add a data entry for D3D10 data for the current MeshSubset
            m_meshSubsetToD3D10[&(*msIt)] = MeshSubsetD3D10();
            MeshSubsetD3D10& newMsD3D10 = m_meshSubsetToD3D10[&(*msIt)];
            newMsD3D10.vtxBuffers.reserve(msIt->vtxAttributes.size());
            newMsD3D10.strides.reserve(msIt->vtxAttributes.size());
            newMsD3D10.offsets.reserve(msIt->vtxAttributes.size());

            ID3D10InputLayout *pNewInputLayout(NULL);
            InputElemDescVector inputDescVec;
            
            // sort the vtxAttributes within each meshSubset by semantic
            sort(msIt->vtxAttributes.begin(), msIt->vtxAttributes.end(), SkinnedMeshD3D10::VtxAttrSemanticLess);
            
            // add D3D10_INPUT_ELEMENT_DESC entries for each vtxAttribute to the inputDescVec
            UINT inputSlot(0);
            int bweightsCnt(0);
            int bindicesCnt(0);
            assert( msIt->vtxAttributes.size() <= D3D10_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT);
            VtxAttrArPtrVector::iterator vtAtIt(msIt->vtxAttributes.begin()), vtAtEnd(msIt->vtxAttributes.end());
            for(; (vtAtIt != vtAtEnd) && (inputSlot < D3D10_IA_VERTEX_INPUT_RESOURCE_SLOT_COUNT); ++vtAtIt)
            {
                DXGI_FORMAT dxFormat = DXGIFormatFromVtxAttr(*vtAtIt);
                assert(dxFormat != DXGI_FORMAT_UNKNOWN);
                if(dxFormat == DXGI_FORMAT_UNKNOWN)
                    continue;

                inputDescVec.push_back(D3D10_INPUT_ELEMENT_DESC());
                D3D10_INPUT_ELEMENT_DESC& inputDesc = inputDescVec.back();
                inputDesc.SemanticName = (*vtAtIt)->m_strSemantic.c_str();
                inputDesc.SemanticIndex = (*vtAtIt)->m_info.m_semIdx;
                inputDesc.Format = dxFormat;
                inputDesc.InputSlot = inputSlot; inputSlot++;
                inputDesc.AlignedByteOffset = D3D10_APPEND_ALIGNED_ELEMENT;
                inputDesc.InputSlotClass = D3D10_INPUT_PER_VERTEX_DATA;
                inputDesc.InstanceDataStepRate = 0;

                VtxAttrArToVtxBufMap::iterator vtxBufIt = m_vtxAtrToVtxBuffers.find(*vtAtIt);
                assert(vtxBufIt != m_vtxAtrToVtxBuffers.end());
                newMsD3D10.vtxBuffers.push_back(vtxBufIt->second);
                newMsD3D10.strides.push_back((*vtAtIt)->m_info.m_compCount * (*vtAtIt)->m_info.m_scalarSize);
                newMsD3D10.offsets.push_back(0);

                // count the number of blendweight/blendindices arrays
                VtxAttributeArray::Semantic sem((*vtAtIt)->m_info.m_semantic);
                if((sem == VtxAttributeArray::BLENDWEIGHTS0) || (sem == VtxAttributeArray::BLENDWEIGHTS1))
                    bweightsCnt++;
                if((sem == VtxAttributeArray::BLENDINDICES0) || (sem == VtxAttributeArray::BLENDINDICES1))
                    bindicesCnt++;
            }

            assert(bweightsCnt == bindicesCnt);
            if(bweightsCnt != bindicesCnt)
                return false;

            // pick a technique based on whether skinning is required or not
            switch(bweightsCnt)
            {
            case 0:
                newMsD3D10.pTechnique = m_pRigidTechnique;
                newMsD3D10.pSoTechnique = m_pRigidSOTechnique;
                break;
            case 1:
                // TODO: add technique to support at most 4 blendweights
                newMsD3D10.pTechnique = NULL;
                break;
            case 2:
            default:
                newMsD3D10.pTechnique = m_pSkinnedTechnique;
                newMsD3D10.pSoTechnique = m_pSkinnedSOTechnique;
                break;
            }
            assert(newMsD3D10.pTechnique);
            if(!newMsD3D10.pTechnique)
                return false;

            D3D10_PASS_DESC passDesc;
            newMsD3D10.pTechnique->GetPassByIndex(0)->GetDesc(&passDesc);
            VRETFALSE(m_pd3dDevice->CreateInputLayout(&inputDescVec[0], static_cast<UINT>(inputDescVec.size()), 
                passDesc.pIAInputSignature, passDesc.IAInputSignatureSize, &pNewInputLayout));

            // add the inputLayout to the global list
            m_inputLayouts.push_back(pNewInputLayout);
            // keep a pointer to the new inputLayout in this subset
            newMsD3D10.pInputLayout = pNewInputLayout;
        }
    }

    // create streamout buffers
    m_SOBuffers.reserve(maxSoBufNum);
    for(int i=0; i<maxSoBufNum; i++)
    {
        ID3D10Buffer *pNewSoBuffer(NULL);

        D3D10_BUFFER_DESC bufferDesc;
        bufferDesc.Usage           = D3D10_USAGE_DEFAULT;
        bufferDesc.ByteWidth       = sizeof(SOVertex) * m_vtxCount;
        bufferDesc.BindFlags       = D3D10_BIND_VERTEX_BUFFER | D3D10_BIND_STREAM_OUTPUT;
        bufferDesc.CPUAccessFlags  = 0;
        bufferDesc.MiscFlags       = 0;

        VRETFALSE(m_pd3dDevice->CreateBuffer(&bufferDesc, NULL, &pNewSoBuffer));

        m_SOBuffers.push_back(pNewSoBuffer);
    }

    // create input layout for streamout buffers
    {
        const D3D10_INPUT_ELEMENT_DESC inputElemDesc[] =
        {
            { "POSITION",  0, DXGI_FORMAT_R32G32B32_FLOAT, 0, D3D10_APPEND_ALIGNED_ELEMENT, D3D10_INPUT_PER_VERTEX_DATA, 0 },
            { "NORMAL",    0, DXGI_FORMAT_R32G32B32_FLOAT, 0, D3D10_APPEND_ALIGNED_ELEMENT, D3D10_INPUT_PER_VERTEX_DATA, 0 },
            { "TEXCOORD",  0, DXGI_FORMAT_R32G32_FLOAT   , 0, D3D10_APPEND_ALIGNED_ELEMENT, D3D10_INPUT_PER_VERTEX_DATA, 0 },
        };
        UINT numElements = sizeof(inputElemDesc)/sizeof(inputElemDesc[0]);

        D3D10_PASS_DESC passDesc;
        m_pRigidTechnique->GetPassByIndex(0)->GetDesc(&passDesc);
        VRETFALSE(m_pd3dDevice->CreateInputLayout(inputElemDesc, numElements, 
            passDesc.pIAInputSignature, passDesc.IAInputSignatureSize, &m_pSOInputLayout));
    }

    // load textures
    TextureList::iterator texIt(m_textures.begin()), texEnd(m_textures.end());
    for(; texIt != texEnd; ++texIt)
    {
        ID3D10ShaderResourceView *pTexSRV(NULL);
        LoadTextureFromFile(m_pd3dDevice, texIt->fileName.c_str(), &pTexSRV);
        m_textureSRVs.push_back(pTexSRV);
        m_textureToSRV[&(*texIt)] = pTexSRV;
    }

    return true;
}


HRESULT SkinnedMeshD3D10::LoadTextureFromFile(ID3D10Device* pd3dDevice, LPCWSTR file, ID3D10ShaderResourceView** pOutTexSRV)
{
    HRESULT hr;

    SAFE_RELEASE(*pOutTexSRV);

    WCHAR str[MAX_PATH];
    V_RETURN(NVUTFindDXSDKMediaFileCch(str, MAX_PATH, file));
    D3DX10_IMAGE_INFO SrcInfo;
    hr = D3DX10GetImageInfoFromFile(str, NULL, &SrcInfo);

    D3DX10_IMAGE_LOAD_INFO texLoadInfo;
    texLoadInfo.Width          = SrcInfo.Width;
    texLoadInfo.Height         = SrcInfo.Height;
    texLoadInfo.Depth          = SrcInfo.Depth;
    texLoadInfo.FirstMipLevel  = 0;
    texLoadInfo.MipLevels      = SrcInfo.MipLevels;
    texLoadInfo.Usage          = D3D10_USAGE_DEFAULT;
    texLoadInfo.BindFlags      = D3D10_BIND_SHADER_RESOURCE;
    texLoadInfo.CpuAccessFlags = 0;
    texLoadInfo.MiscFlags      = SrcInfo.MiscFlags;
    texLoadInfo.Format         = SrcInfo.Format;
    texLoadInfo.Filter         = D3DX10_FILTER_TRIANGLE;
    texLoadInfo.MipFilter      = D3DX10_FILTER_TRIANGLE;
    texLoadInfo.pSrcInfo       = &SrcInfo;

    ID3D10Resource *pRes = NULL;

    V_RETURN( D3DX10CreateTextureFromFile(pd3dDevice, str, &texLoadInfo, NULL, &pRes ) );
    if( pRes )
    {
        ID3D10Texture2D* texture;

        pRes->QueryInterface( __uuidof( ID3D10Texture2D ), (LPVOID*)&texture );
        D3D10_TEXTURE2D_DESC desc;
        texture->GetDesc( &desc );
        D3D10_SHADER_RESOURCE_VIEW_DESC SRVDesc;
        ZeroMemory( &SRVDesc, sizeof(SRVDesc) );
        SRVDesc.Format = desc.Format;
        SRVDesc.ViewDimension = D3D10_SRV_DIMENSION_TEXTURE2D;
        SRVDesc.Texture2D.MostDetailedMip = 0;
        SRVDesc.Texture2D.MipLevels = desc.MipLevels;

        V_RETURN (pd3dDevice->CreateShaderResourceView( texture, &SRVDesc, pOutTexSRV));

        SAFE_RELEASE( texture );
     }

    SAFE_RELEASE(pRes);
    return S_OK;
}


bool SkinnedMeshD3D10::SetRenderMatrices(float *pWorldMatrix, float *pViewMatrix, float *pProjectionMatrix)
{
    HRESULT hr;

    assert(pWorldMatrix);
    assert(pViewMatrix);
    assert(pProjectionMatrix);
    if(!pWorldMatrix || !pViewMatrix || !pProjectionMatrix)
        return false;

    VRETFALSE(m_pWorldVar->SetMatrix(pWorldMatrix));

    FMMatrix44 *pView = reinterpret_cast<FMMatrix44*>(pViewMatrix);
    FMMatrix44 *pProjection = reinterpret_cast<FMMatrix44*>(pProjectionMatrix);
    FMMatrix44 viewProjectionMatrix = (*pProjection) * (*pView);

    FMMatrix44 cameraMatrix = pView->Inverted();
    FMVector3 eyePos = cameraMatrix.GetTranslation();
    m_pEyeVar->SetFloatVector(eyePos);

    VRETFALSE(m_pViewProjectionVar->SetMatrix(viewProjectionMatrix));
    
    // compute and set worldViewProjection matrix 
    FMMatrix44 *pWorld = reinterpret_cast<FMMatrix44*>(pWorldMatrix);
    FMMatrix44 worldViewProjectionMatrix = viewProjectionMatrix * (*pWorld);

    return true;
}

bool SkinnedMeshD3D10::Render(float *pWorldMatrix, float *pViewMatrix, float *pProjectionMatrix,
                              unsigned int  instanceCount)
{

    bool ret = SetRenderMatrices(pWorldMatrix, pViewMatrix, pProjectionMatrix);
    if(!ret)
        return false;

    m_instanceCount = instanceCount;

    return RenderRootNodes(pWorldMatrix);
}

bool SkinnedMeshD3D10::RenderToSO(float *pWorldMatrix, unsigned int dstSoBufNum)
{
    if(dstSoBufNum >= m_SOBuffers.size() )
        return false;

    m_dstSoBuffer = dstSoBufNum;
    m_instanceCount = 0;
    m_IsRenderingToSO = true;

    bool ret = RenderRootNodes(pWorldMatrix);

    m_pd3dDevice->SOSetTargets(0, NULL, NULL);

    m_IsRenderingToSO = false;
    
    return ret;
}

bool SkinnedMeshD3D10::RenderFromSO(ID3D10EffectTechnique *pTechnique, unsigned int instanceCount)
{
    assert(pTechnique);
    assert(pTechnique->IsValid());
    if(!pTechnique)
        return false;

    m_pFromSOTechnique = pTechnique;
    m_instanceCount = instanceCount;
    m_IsRenderingFromSO = true;

    bool ret = RenderRootNodes(NULL);

    m_pFromSOTechnique = NULL;
    m_instanceCount = 0;
    m_IsRenderingFromSO = false;

    return ret;
}

bool SkinnedMeshD3D10::RenderFromSO(unsigned int srcSoBufNum, ID3D10EffectTechnique *pTechnique,
                                    unsigned int instanceCount)
{
    if(srcSoBufNum >= m_SOBuffers.size())
        return false;

    m_srcSoBuffer = srcSoBufNum;

    return RenderFromSO(pTechnique, instanceCount);
}

bool SkinnedMeshD3D10::RenderFromSO(ID3D10InputLayout *pInputLayout, ID3D10EffectTechnique *pTechnique,
                                    unsigned int instanceCount)
{
    assert(pInputLayout);
    if(!pInputLayout)
        return false;

    m_pTmpSOInputLayout = pInputLayout;

    bool ret = RenderFromSO(pTechnique, instanceCount);

    m_pTmpSOInputLayout = NULL;
    
    return ret;
}

void SkinnedMeshD3D10::SetRenderFilterExclude(stdStringVector *excludeNodes)
{
    if(excludeNodes)
        m_pIncludeNodes = NULL;
    m_pExcludeNodes = excludeNodes;
}

void SkinnedMeshD3D10::SetRenderFilterInclude(stdStringVector *includeNodes)
{
    if(includeNodes)
        m_pExcludeNodes = NULL;
    m_pIncludeNodes = includeNodes;
}


ID3D10Buffer *SkinnedMeshD3D10::GetSOBuffer(unsigned int soBufNum)
{
    if(soBufNum > m_SOBuffers.size())
        return NULL;
    
    return m_SOBuffers[soBufNum];
}


bool SkinnedMeshD3D10::RenderRootNodes(float *pWorldMatrix)
{
    // Traverse hiearchy rendering the "instances" of any node
    //   as either static mesh (update worldView matrix)
    //   or skinned mesh (update worldView matrix + skinning matrices)
    //   each instance may have multiple subsets, each with a different material
    NodeList::iterator nodeIt(m_nodes.begin()), nodeEnd(m_nodes.end());
    for( ; nodeIt != nodeEnd; ++nodeIt)
    {
        if(nodeIt->pParent == NULL)
            if(!RenderNode(&(*nodeIt), pWorldMatrix))
                return false;
    }

    return true;
}

bool SkinnedMeshD3D10::RenderNode(Node *pNode, float *pWorldMatrix)
{
    HRESULT hr;
    assert(pNode);
    assert(m_IsRenderingFromSO || pWorldMatrix);

    // Decide whether this node must be rendered or not
    bool skipNodeRender(true);
    if(pNode->instances.size() > 0)
    {
        if(m_pIncludeNodes)
        {
            skipNodeRender = true;

            stdStringVector::iterator strVecIt(m_pIncludeNodes->begin()),
                strVecEnd(m_pIncludeNodes->end());
            for(; strVecIt != strVecEnd; ++strVecIt)
            {
                if( pNode->id == (*strVecIt) )
                {
                    skipNodeRender = false;
                    break;
                }
            }
        }
        else
        {
            skipNodeRender = false;

            if(m_pExcludeNodes)
            {
                stdStringVector::iterator strVecIt(m_pExcludeNodes->begin()),
                    strVecEnd(m_pExcludeNodes->end());
                for(; strVecIt != strVecEnd; ++strVecIt)
                {
                    if( pNode->id == (*strVecIt) )
                    {
                        skipNodeRender = true;
                        break;
                    }
                }
            }
        }
    }

    if(!skipNodeRender)
    {
        // set the world matrix
        if(!m_IsRenderingFromSO)
        {
            FMMatrix44 *pWorld = reinterpret_cast<FMMatrix44*>(pWorldMatrix);
            FMMatrix44 worldMatrix = (*pWorld) * pNode->worldXForm;
            VRETFALSE(m_pWorldVar->SetMatrix(worldMatrix));
        }

        MeshInstanceList::iterator instIt(pNode->instances.begin()), instEnd(pNode->instances.end());
        for(; instIt != instEnd; ++instIt)
        {
            if(instIt->pMesh->hasSkin)
            {
                assert(instIt->bones.size() > 0);

                typedef std::vector<FMMatrix44> FMMatrix44Vector;
                FMMatrix44 boneMatrix;
                FMMatrix44Vector boneMatrices;
                boneMatrices.reserve(instIt->bones.size());
                NodeBindingVector::iterator boneIt(instIt->bones.begin()), boneEnd(instIt->bones.end());
                for(;boneIt != boneEnd; ++boneIt)
                {
                    boneMatrix = boneIt->node->worldXForm * boneIt->invBindPose * instIt->meshBindPose;
                    boneMatrices.push_back(boneMatrix);
                }

                VRETFALSE(m_pBonesVar->SetMatrixArray((float*)&boneMatrices[0], 0, static_cast<UINT>(boneMatrices.size())));
                
            }

            MeshSubsetPtrList::iterator msIt(instIt->pMesh->subsets.begin()),
                msEnd(instIt->pMesh->subsets.end());
            for(; msIt != msEnd; ++msIt)
            {
                MeshSubsetToMSD3D10Map::iterator msD3D10It(m_meshSubsetToD3D10.find(*msIt));
                assert(msD3D10It != m_meshSubsetToD3D10.end());
                if( msD3D10It != m_meshSubsetToD3D10.end() )
                {
                    MeshSubsetD3D10 &msD3D10 = msD3D10It->second;

                    // apply the appropriate technique
                    ID3D10EffectTechnique *pTechnique(NULL);
                    if( m_IsRenderingToSO )
                    {
                        pTechnique = msD3D10.pSoTechnique;
                    }
                    else
                    {
                        if( m_IsRenderingFromSO)
                            pTechnique = m_pFromSOTechnique;
                        else
                            pTechnique = msD3D10.pTechnique;
                    }
                    assert(pTechnique && pTechnique->IsValid());
                    
                    if( m_IsRenderingToSO )
                    {
                        UINT offset = instIt->pMesh->vtxBegin * sizeof(SOVertex);
                        
                        pTechnique->GetPassByIndex(0)->Apply(0);

                        m_pd3dDevice->SOSetTargets(1, &m_SOBuffers[m_dstSoBuffer], &offset);

                        m_pd3dDevice->IASetInputLayout(msD3D10.pInputLayout);
                        m_pd3dDevice->IASetPrimitiveTopology( D3D10_PRIMITIVE_TOPOLOGY_POINTLIST );
                        m_pd3dDevice->IASetVertexBuffers( 0, static_cast<UINT>(msD3D10.vtxBuffers.size()),
                            &msD3D10.vtxBuffers[0], &msD3D10.strides[0], &msD3D10.offsets[0] );
                        m_pd3dDevice->Draw(instIt->pMesh->vtxCount, instIt->pMesh->vtxBegin);
                        // we break out of the meshSubset loop because we are processing
                        //   all the vertices shared by all meshSubsets of the given mesh
                        break;
                    }
                    else
                    {
                        // set meshSubset material textures
                        BOOL hasNormalMap(FALSE);
                        MsPtrToMatPtrMap::iterator msToMatIt(instIt->subsetToMaterial.find(*msIt));
                        if( msToMatIt != instIt->subsetToMaterial.end())
                        {
                            BoundTextureList &bts = msToMatIt->second->boundTextures;
                            BoundTextureList::iterator btIt(bts.begin()), btEnd(bts.end());
                            for(; btIt != btEnd; ++btIt)
                            {
                                if(!btIt->pTexture || (btIt->semantic == BoundTexture::UNKNOWN))
                                    continue;

                                ID3D10EffectShaderResourceVariable *pSRVVar(NULL);

                                switch(btIt->semantic)
                                {
                                case BoundTexture::DIFFUSE:
                                    pSRVVar = m_pDiffuseTexVar;
                                    break;
                                case BoundTexture::SPECULAR:
                                    pSRVVar = m_pSpecularTexVar;
                                    break;
                                case BoundTexture::NORMAL:
                                    pSRVVar = m_pNormalTexVar;
                                    hasNormalMap = TRUE;
                                    break;
                                default:
                                    break;
                                }
                                if(pSRVVar)
                                {
                                    TextureToSRVMap::iterator texSRVIt = m_textureToSRV.find(btIt->pTexture);
                                    assert(texSRVIt != m_textureToSRV.end());
                                    VRETFALSE(pSRVVar->SetResource(texSRVIt->second));
                                }
                            }
                        }
                        VRETFALSE(m_pUserNormalMapVar->SetBool(hasNormalMap));

                        pTechnique->GetPassByIndex(0)->Apply(0);

                        // pick the right vertex buffer and input layout
                        if( m_IsRenderingFromSO)
                        {
                            if(m_pTmpSOInputLayout)
                            {
                                m_pd3dDevice->IASetInputLayout(m_pTmpSOInputLayout);
                                // vertex buffers are set by the caller
                            }
                            else
                            {
                                m_pd3dDevice->IASetInputLayout(m_pSOInputLayout);
                                UINT stride = sizeof(SOVertex);
                                UINT offset = 0;
                                m_pd3dDevice->IASetVertexBuffers(0, 1, 
                                    &m_SOBuffers[m_srcSoBuffer], &stride, &offset);
                            }
                        }
                        else
                        {
                            m_pd3dDevice->IASetInputLayout(msD3D10.pInputLayout);
                            m_pd3dDevice->IASetVertexBuffers( 0, static_cast<UINT>(msD3D10.vtxBuffers.size()),
                                &msD3D10.vtxBuffers[0], &msD3D10.strides[0], &msD3D10.offsets[0] );
                        }

                        m_pd3dDevice->IASetPrimitiveTopology( D3D10_PRIMITIVE_TOPOLOGY_TRIANGLELIST );
                        m_pd3dDevice->IASetIndexBuffer(m_pIndexBuffer, DXGI_FORMAT_R32_UINT, 0);
                        if( m_instanceCount == 0 )
                        {
                            m_pd3dDevice->DrawIndexed( (*msIt)->idxCount, (*msIt)->idxBegin, 0);
                        }
                        else
                        {
                            m_pd3dDevice->DrawIndexedInstanced( (*msIt)->idxCount, m_instanceCount,
                                (*msIt)->idxBegin, 0, 0);
                        }
                    }
                }
            }
        }
    }

    // render the children
    NodePtrList::iterator childIt(pNode->children.begin()), childEnd(pNode->children.end());
    for( ; childIt != childEnd; ++childIt)
    {
        RenderNode(*childIt, pWorldMatrix);
    }

    return true;
}

// END DX10 Rendering Code
//////////////////////////////////////////////////////////////////////////////
