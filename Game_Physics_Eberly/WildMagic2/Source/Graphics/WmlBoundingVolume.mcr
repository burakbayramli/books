// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLBOUNDINGVOLUME_MCR
#define WMLBOUNDINGVOLUME_MCR

#define WmlDeclareBV(classname) \
public: \
    static int RegisterFactory (); \
protected: \
    static BoundingVolume* Create (); \
    static BoundingVolume* Create (int iVertexCount, \
        const Vector3f* akVertex, const int* aiConnect, int i0, int i1, \
        int* aiISplit, Vector3f& rkOrigin, Vector3f& rkDirection)

#define WmlImplementBV(classname,bvtype) \
    int classname::RegisterFactory () \
    { \
        classname::ms_aoCreatorS[BoundingVolume::bvtype] = \
            &classname::Create; \
        classname::ms_aoCreatorT[BoundingVolume::bvtype] = \
            &classname::Create; \
        return 0; \
    }

// Insert in derived class header file.  Forces pre-main registration of
// factory whenever the header file is included in an application source file.
#define WmlRegisterBV(classname) \
    static int _##classname##registerFactory = classname::RegisterFactory ()

#endif
