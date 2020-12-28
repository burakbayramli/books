//----------------------------------------------------------------------------------
// File:   SkinnedMesh.h
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

#ifndef _SKINNED_MESH_H_
#define _SKINNED_MESH_H_

//----------------------------------------------------------------------------------
// SkinnedMesh Interface and "factory"
//----------------------------------------------------------------------------------

#include <vector>
class SkinnedMesh;

class SkinnedMesh
{
public:
    typedef std::vector<std::string> stdStringVector;

    // Creation
    static SkinnedMesh *CreateD3D10FromColladaFile(LPCTSTR filename,
        ID3D10Device* pd3dDevice, int maxSoBufNum);
    
    // Rendering
    //   Instancing is used if "instanceCount" > 0
    virtual bool Render(float *pWorldMatrix, float *pViewMatrix, float *pProjectionMatrix,
        unsigned int  instanceCount = 0) = 0;
    
    virtual bool RenderToSO(float *pWorldMatrix, unsigned int dstSoBufNum) = 0;
    virtual bool RenderFromSO(unsigned int srcSoBufNum, ID3D10EffectTechnique *pTechnique, 
        unsigned int instanceCount = 0) = 0; 
    virtual bool RenderFromSO(ID3D10InputLayout *pInputLayout, ID3D10EffectTechnique *pTechnique, 
        unsigned int instanceCount = 0) = 0;


    // Render filters: these are mutually excluse filters. They define a list of
    //   nodes to be rendered.
    //   Default: no filters, all nodes are rendered
    //   Exclude: all nodes except the ones with id matching any of those listed is rendered
    //   Include: only the nodes with id matching any of those listed is rendered
    virtual void SetRenderFilterExclude(stdStringVector *excludeNodes) = 0;
    virtual void SetRenderFilterInclude(stdStringVector *includeNodes) = 0;

    virtual ID3D10Buffer *GetSOBuffer(unsigned int soBufNum) = 0;

    // Animation controls
    enum ANIMATION_END_HANDLING
    {
        BLOCK,
        LOOP,
    };

    virtual void Update(float elapsedTime) = 0;
    virtual void SetStartEndTimes(float startTime, float endTime) = 0;
    virtual void GetStartEndTimes(float *outStartTime, float *outEndTime) = 0;
    virtual void SetCurrentTime(float currentTime) = 0;
    virtual void SetAnimationEndHandling(ANIMATION_END_HANDLING aeh) = 0;

    // Destructor
    virtual ~SkinnedMesh();

protected:
    virtual bool LoadFromColladaFile(LPCTSTR filename) = 0;
    SkinnedMesh();
};


//----------------------------------------------------------------------------------
// SkinnedMeshImpl: implements loading and animation
//----------------------------------------------------------------------------------

#undef SAFE_DELETE
#undef SAFE_DELETE_ARRAY
#undef SAFE_RELEASE
#include "FCollada.h"
#include "FCDocument/FCDocument.h"
#include "FCDocument/FCDGeometry.h"
#include "FCDocument/FCDGeometryMesh.h"
#include "FCDocument/FCDGeometryPolygons.h"
#include "FCDocument/FCDGeometryPolygonsTools.h"
#include <string>
#include <list>
#include <vector>
#include <map>
class FCDSceneNode;
class FCDEntityInstance;
class FCDMaterialInstance;
class FCDMaterial;
class FCDImage;


class SkinnedMeshImpl : public SkinnedMesh
{
public:
    virtual ~SkinnedMeshImpl();

    // Animation Controls
    virtual void Update(float elapsedTime);
    virtual void SetStartEndTimes(float startTime, float endTime);
    virtual void GetStartEndTimes(float *outStartTime, float *outEndTime);
    virtual void SetCurrentTime(float currentTime);
    virtual void SetAnimationEndHandling(ANIMATION_END_HANDLING aeh);

protected:
    friend SkinnedMesh;

    SkinnedMeshImpl();

    bool LoadFromColladaFile(LPCTSTR filename);

    // Assumptions:
    //
    // Input is triangle mesh, with a given number of vertex, a number of attributes per vertex
    //  and a numer of triangles (a list of indices into the vertex arrays). 
    //
    // The mesh may contain multiple subsets of faces affected by a different material.
    //
    // Attributes may be Position, Normal, Tangent, Binormal/Bitangent, TexCoord#, Color#, 
    //  InfluenceWeight#, InfluenceIdx# (for Skinning), etc.
    //
    // Input also contains a hierarchy of scene nodes, each with a matrix xform that may be
    //  animated. If animated, it is stored as a list of matrices for different keyframes.
    //
    // Materials may contain multiple texture maps, such as diffuse, specular, normal.
    //
    // All the geometries in the geometry library are loaded into a common list of vertex attribute 
    //  arrays, such that there is only a single array for positions, a single array for normals, etc.
    //  So if a subset of geometry doesn't have a particular attribute that has been already created
    //  for other geometry that attribute array is padded with zeroes.
    
    // Custom data structures to store the data
    // ========================================
    struct Node;
    class VtxAttributeArray;

    typedef std::vector<VtxAttributeArray*> VtxAttrArPtrVector;
    
    typedef FMMatrix44 Matrix44;
    typedef std::map<float, Matrix44> FloatToMatrix44Map;

    struct Texture
    {
        std::string         id;
        fstring             fileName;
    };
    typedef std::list<Texture> TextureList;

    class BoundTexture
    {
    public:
        enum Semantic { DIFFUSE, SPECULAR, NORMAL, COUNT, UNKNOWN = -1 };
        BoundTexture(Texture *pTex, uint32 sem);
        Semantic            semantic;
        Texture             *pTexture;
    protected:
        BoundTexture() {}
    };
    typedef std::list<BoundTexture> BoundTextureList;

    struct Material
    {
        std::string         id;
        BoundTextureList    boundTextures;
    };
    typedef std::list<Material> MaterialList;

    struct MeshSubset
    {
        // begin and count within m_indices arrays
        uint32              idxBegin;
        uint32              idxCount;
        // mesh subsets within a Mesh may use different # of attributes
        VtxAttrArPtrVector  vtxAttributes;
        // mesh subsets will be bound at instantiation time to a given material
        fstring             materialSemantic;
    };
    typedef std::list<MeshSubset> MeshSubsetList;
    typedef std::list<MeshSubset*> MeshSubsetPtrList;
    typedef std::map<MeshSubset*,Material*> MsPtrToMatPtrMap;

    struct Mesh
    {
        std::string         id;
        // begin and count within the multiple equal-sized m_vtxAttributes arrays
        uint32              vtxBegin;
        uint32              vtxCount;
        MeshSubsetPtrList   subsets;
        bool                hasSkin;
        VtxAttrArPtrVector  vtxAttributes;  // all the vtx attributes used by all the mesh subsets
        FCDGeometryIndexTranslationMap oldIdxToNewIdxs;

        // bounding sphere
        FMVector3           sphereCenter;
        float               sphereRadius;
    };
    typedef std::list<Mesh> MeshList;

    struct NodeBinding
    {
        std::string sid;
        Node        *node;
        Matrix44    invBindPose;
    };
    typedef std::vector<NodeBinding> NodeBindingVector;

    struct MeshInstance
    {
        Mesh                *pMesh;
        MsPtrToMatPtrMap    subsetToMaterial;
        NodeBindingVector   bones;
        Matrix44            meshBindPose;

        // bounding sphere
        FMVector3           sphereCenter;
        float               sphereRadius;
    };
    typedef std::list<MeshInstance>MeshInstanceList;

    typedef std::list<Node*>NodePtrList;
    typedef std::list<Node> NodeList;
    struct Node
    {
        std::string         id;
        std::string         sid;
        Node                *pParent;
        NodePtrList         children;

        Matrix44            xform;      // the local xform, relative to the parent
        Matrix44            worldXForm; // the world xform, composed with the parent's xform

        // animation
        FloatToMatrix44Map  xformKeyFrames;
        float               minKeyTime;
        float               maxKeyTime;

        // for Rendering
        MeshInstanceList    instances;

        void EvaluateLocalXForm(float timeValue);
    };

    class VtxAttributeArray
    {
    public:
        enum ScalarType { ST_FLOAT, ST_SINT32, ST_UINT32, ST_SNORM32, ST_UNORM32,
                            ST_SINT8, ST_UINT8, ST_SNORM8, ST_UNORM8, SCALAR_TYPE_COUNT };

        enum Semantic { POSITION, VERTEX, NORMAL, GEOTANGENT, GEOBINORMAL,
		    TEXCOORD, TEXTANGENT, TEXBINORMAL, UV, COLOR, EXTRA, // Maya-specific, used for blind data
            BLENDINDICES0, BLENDINDICES1, BLENDWEIGHTS0, BLENDWEIGHTS1,
		    UNKNOWN = -1,
        };

        VtxAttributeArray(ScalarType type, uint32 compCount, 
            const std::string &strSemantic, Semantic semantic, uint32 semanticIdx = 0);
        virtual ~VtxAttributeArray();

        bool AppendData(uint32 compCount, uint32 scalarCount, void *srcData);

        float           * AsFloat(int idx = 0)  { return &(static_cast<float*>(m_pData))[idx*m_info.m_compCount]; }
        int32           * AsSInt32(int idx = 0) { return &(static_cast<int32*>(m_pData))[idx*m_info.m_compCount]; }
        uint32          * AsUInt32(int idx = 0) { return &(static_cast<uint32*>(m_pData))[idx*m_info.m_compCount]; }
        int8            * AsSInt8(int idx = 0)  { return &(static_cast<int8*>(m_pData))[idx*m_info.m_compCount]; }
        uint8           * AsUInt8(int idx = 0)  { return &(static_cast<uint8*>(m_pData))[idx*m_info.m_compCount]; }
        
        std::string         m_strSemantic;  // semantic string (to differentiate in case it's UNKNOWN)
        
        struct VtxAttributeArrayInfo
        {
            VtxAttributeArrayInfo( Semantic semantic,
                                   uint32 semanticIdx,
                                   ScalarType type,
                                   uint32 compCount );

            Semantic            m_semantic;     // semantic enumerant for well known semantics
            uint32              m_semIdx;       // semantic index, for attributes that need more than 4 floats
            ScalarType          m_type;         // data type of scalar value for each component
            uint32              m_scalarSize;   // number of bytes per scalar
            uint32              m_compCount;    // number of scalar components per attribute

        };

        struct VtxAttributeArrayInfoLess
        {
            bool operator() (const VtxAttributeArrayInfo &pLeft, const VtxAttributeArrayInfo &pRight) const;
        };

        VtxAttributeArrayInfo   m_info;

        uint32              m_count;        // number of attributes in this array

    protected:
        VtxAttributeArray();

        void                *m_pData;       // a pointer to the buffer containing the attribute data array

    };
    typedef std::list<VtxAttributeArray> VtxAttributeArrayList;
    //typedef std::map<VtxAttributeArray::Semantic, VtxAttributeArray *> SemanticToVtxAttrArMap;
    typedef std::map<VtxAttributeArray::VtxAttributeArrayInfo, VtxAttributeArray *,
        VtxAttributeArray::VtxAttributeArrayInfoLess> SemanticToVtxAttrArMap;
    typedef std::map<uint32, SkinnedMeshImpl::VtxAttributeArray::Semantic> FSemanticToSemanticMap;

    typedef std::vector<uint32> IndexBuffer;

    typedef std::map<std::string, Mesh*>        IdToMeshMap;
    typedef std::map<std::string, Material*>    IdToMaterialMap;
    typedef std::map<std::string, Texture*>     IdToTextureMap;

    
    
    // Data holders
    // ============

    // arrays of vertex attributes
    VtxAttributeArrayList   m_vtxAttributes;
    SemanticToVtxAttrArMap  m_semToVtxAttrAr;
    FSemanticToSemanticMap  m_fsemToSemMap;
    // index buffer
    IndexBuffer             m_indices;

    // mesh subsets associate a subset of mesh triangles with a material
    MeshSubsetList          m_meshSubsets;
    MeshList                m_meshes;
    IdToMeshMap             m_idToMesh;
    

    // library of textures
    TextureList             m_textures;
    IdToTextureMap          m_idToTexture;
    // library of materials: each material contain references to textures, shaders, etc
    MaterialList            m_materials;
    IdToMaterialMap         m_idToMaterial;

    // The nodes (includings bones/joints) with animation data
    NodeList                m_nodes;

    uint32                  m_vtxCount;
    uint32                  m_triCount;


    // Animation State
    //================
    ANIMATION_END_HANDLING  m_animEndHandling;  // how should we update m_currentTime when we reach the end
    float                   m_currentTime;  // the time after the last call to Update or SetCurrentTime
    float                   m_startTime;    // animation start time, may be set by the user
    float                   m_endTime;      // the animation end time, may be set by the user, 
                                            //   enforced to be greater or equal to startTime
    
    float                   m_minTime;      // minimum time across all keyframes in all the Node animations
    float                   m_maxTime;      // maximum time across all keyframes in all the Node animations
    float                   m_lastNodesUpdateTime;  // the last time value at which all nodes where updated
    

    // Internal functions
    // ==================
    // Perform a mesh self-consistency check and return true if the mesh is self-consistent
    bool IsSelfConsistent(void);
    // Perform some Initialization
    void InitFsemToSemMap(void);

    Node * AddNode(FCDSceneNode *pSrcSceneNode, Node *pParentNode);
    bool ProcessNode(FCDSceneNode *pSrcSceneNode, Node *pParentNode);
    bool ProcessGeometryInstNode(FCDEntityInstance *pEntityInstance, Node *pDstNode);
    bool ProcessControllerInstNode(FCDEntityInstance *pEntityInstance, Node *pDstNode);
    Mesh * GetGeometry(FCDEntity *pEntity);
    Mesh * AddGeometry(FCDEntity *pEntity);
    void GenSubsetToMaterialMap(MsPtrToMatPtrMap &dstSubsetToMaterialMap, 
            const MeshSubsetPtrList &subsetPtrs, FCDEntityInstance *pGeomInst);
    Material * GetMaterial(FCDMaterialInstance *pMatInst);
    Material * AddMaterial(FCDMaterialInstance *pMatInst);
    Texture * GetTexture(FCDImage *pImage);
    Texture * AddTexture(FCDImage *pImage);

    void UpdateNodes(float timeValue);
    void UpdateNode(Node *pNode, float timeValue);

    VtxAttributeArray * GetVtxAttributeArray(VtxAttributeArray::ScalarType type, uint32 compCount, 
        const std::string &strSemantic, VtxAttributeArray::Semantic semantic, uint32 semanticIdx = 0);
};


//----------------------------------------------------------------------------------
// SkinnedMeshD3D10 - rendering implementation for Direct3D 10
//----------------------------------------------------------------------------------

class SkinnedMeshD3D10 : public SkinnedMeshImpl
{
public:
    virtual ~SkinnedMeshD3D10();

    virtual bool Render(float *pWorldMatrix, float *pViewMatrix, float *pProjectionMatrix,
        unsigned int instanceCount);
    virtual bool RenderToSO(float *pWorldMatrix, unsigned int dstSoBufNum);
    virtual bool RenderFromSO(unsigned int srcSoBufNum, ID3D10EffectTechnique *pTechnique,
        unsigned int instanceCount);
    virtual bool RenderFromSO(ID3D10InputLayout *pInputLayout, ID3D10EffectTechnique *pTechnique,
        unsigned int instanceCount);

    virtual void SetRenderFilterExclude(stdStringVector *excludeNodes);
    virtual void SetRenderFilterInclude(stdStringVector *includeNodes);

    virtual ID3D10Buffer *GetSOBuffer(unsigned int soBufNum);

    // A simple vertex type used in the streamout buffers
    struct SOVertex
    {
        float Pos[3];
        float Norm[3];
        float Tex[2];
    };

protected:
    // Constructor is protected to ensure the use SkinnedMesh::CreateD3D10FromColladaFile 
    SkinnedMeshD3D10();
    // SkinnedMesh must be friend so that it can create a SkinnedMeshD3D10
    friend SkinnedMesh;


    // Custom types for resource storage
    typedef std::vector<ID3D10Buffer*> ID3D10BufferVector;
    typedef std::vector<ID3D10InputLayout*> ID3D10InputLayoutVector;
    typedef std::vector<ID3D10ShaderResourceView*> ID3D10SRVVector;
    typedef std::vector<UINT> UINTVector;
    struct MeshSubsetD3D10
    {
        ID3D10EffectTechnique   *pTechnique;
        ID3D10EffectTechnique   *pSoTechnique;
        ID3D10InputLayout       *pInputLayout;
        ID3D10BufferVector      vtxBuffers;
        UINTVector              strides;
        UINTVector              offsets;
    };
    typedef std::map<VtxAttributeArray*, ID3D10Buffer*> VtxAttrArToVtxBufMap;
    typedef std::map<MeshSubset*, MeshSubsetD3D10> MeshSubsetToMSD3D10Map;
    typedef std::map<Texture*, ID3D10ShaderResourceView*> TextureToSRVMap;

    ID3D10Device                *m_pd3dDevice;

    ID3D10Buffer                *m_pIndexBuffer;
    ID3D10BufferVector          m_vtxBuffers;
    ID3D10InputLayoutVector     m_inputLayouts;

    ID3D10SRVVector             m_textureSRVs;

    ID3D10BufferVector          m_SOBuffers;
    ID3D10InputLayout           *m_pSOInputLayout;
    ID3D10InputLayout           *m_pTmpSOInputLayout;

    VtxAttrArToVtxBufMap        m_vtxAtrToVtxBuffers;
    MeshSubsetToMSD3D10Map      m_meshSubsetToD3D10;
    TextureToSRVMap             m_textureToSRV;

    ID3D10Effect                *m_pEffect;
    ID3D10EffectTechnique       *m_pSkinnedTechnique;
    ID3D10EffectTechnique       *m_pRigidTechnique;
    ID3D10EffectTechnique       *m_pSkinnedSOTechnique;
    ID3D10EffectTechnique       *m_pRigidSOTechnique;
    
    ID3D10EffectTechnique       *m_pFromSOTechnique;

    ID3D10EffectMatrixVariable  *m_pWorldVar;
    ID3D10EffectMatrixVariable  *m_pViewProjectionVar;
    ID3D10EffectMatrixVariable  *m_pBonesVar;
    ID3D10EffectVectorVariable  *m_pEyeVar;

    ID3D10EffectShaderResourceVariable *m_pDiffuseTexVar;
    ID3D10EffectShaderResourceVariable *m_pSpecularTexVar;
    ID3D10EffectShaderResourceVariable *m_pNormalTexVar;
    
    ID3D10EffectScalarVariable  *m_pUserNormalMapVar;

    unsigned int                m_instanceCount;

    // For rendering from/to Streamout 
    bool                        m_IsRenderingToSO;
    bool                        m_IsRenderingFromSO;
    unsigned int                m_dstSoBuffer;
    unsigned int                m_srcSoBuffer;

    // For filtering of scene graph nodes
    stdStringVector             *m_pIncludeNodes;
    stdStringVector             *m_pExcludeNodes;

    // Internal functions
    // ==================

    DXGI_FORMAT DXGIFormatFromVtxAttr(VtxAttributeArray *pVtxAtAr);

    virtual bool InitializeForRendering(ID3D10Device* pd3dDevice, int maxSoBufNum);
    virtual bool SetRenderMatrices(float *pWorldMatrix, float *pViewMatrix, float *pProjectionMatrix);
    virtual bool RenderFromSO(ID3D10EffectTechnique *pTechnique, unsigned int instanceCount);
    virtual bool RenderRootNodes(float *pWorldMatrix);
    virtual bool RenderNode(Node *pNode, float *pWorldMatrix);

public:
    static HRESULT LoadTextureFromFile(ID3D10Device* pd3dDevice, LPCWSTR file, ID3D10ShaderResourceView** pOutTexSRV);

    static bool SkinnedMeshD3D10::VtxAttrSemanticLess(VtxAttributeArray *pLeft, VtxAttributeArray *pRight);
};

#endif // _SKINNED_MESH_H_
