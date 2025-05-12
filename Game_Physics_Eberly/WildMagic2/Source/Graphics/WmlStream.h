// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLSTREAM_H
#define WMLSTREAM_H

// The streaming system writes data to disk in Little Endian format.  This is
// the format for the PC.  In order to support copying scene graph files
// (*.mgc) files between the PC and platforms that use Big Endian format, you
// need to define WML_BIG_ENDIAN.  For swapping to Big Endian format, native
// types are assumed to be of the following sizes (effectively what you get
// on most 32-bit systems):
//
//   type                  | size
//   ----------------------+------
//   char, unsigned char   |    1
//   short, unsigned short |    2
//   int, unsigned int     |    4
//   long, unsigned long   |    4
//   float                 |    4
//   double                |    8
//   bool                  |    1
//   enumerated values     | <= 4
//   pointers              |    4
//
// Read/write functions are provided for streaming Boolean as 1-byte values
// (0 or 1) and enumerated values as 4-byte integers.  This should take care
// of compilers that define 'bool' to have a size different than 1-byte.  It
// also takes care of compilers such as CodeWarrior on the Macintosh that
// likes to select the smallest size possible to represent enumerated values.
// The only pointers that are read/written are Object* values.  There will be
// a problem on systems that represent these using a different number than 4
// bytes.  I will fix the portability problem if I get requests from users to
// do so.
//
// Byte-swapping of nonvirtual, non-Object-derived classes Bound, ColorRGB,
// Matrix2, Matrix3, Plane, Vector2, and Vector3 are handled by specialized
// templates.  Any other such classes added to the system will require similar
// specialization.

#include "WmlStringTree.h"
#include "WmlVersion.h"
#include <map>
#include <vector>

namespace Wml
{

class Object;

class WML_ITEM Stream
{
public:
    // construction and destruction
    Stream ();
    ~Stream ();

    // The objects to process, each object representing an entry into a
    // connected component of the abstract graph.
    bool Insert (Object* pkObject);
    bool Remove (Object* pkObject);
    void RemoveAll ();
    int GetObjectCount ();
    Object* GetObjectAt (int i) const;
    bool IsTopLevel (Object* pkObject);

    // memory loads and saves
    bool Load (char* acBuffer, int iSize, int iNext = 0);
    bool Save (char*& racBuffer, int& riSize);

    // file loads and saves
    bool Load (const char* acFilename);
    bool Save (const char* acFilename);

    // file save (ASCII text)
    bool SaveText (const char* acFilename, int iTabSize = 4);

    // support for memory and disk usage
    int GetMemoryUsed ();
    int GetDiskUsed ();

    // The version of the last loaded file from disk.  If no file has been
    // loaded, the returned values are -1.
    Version GetVersion () const;


    //*** BEGIN INTERNAL USE ONLY ***

    // linking support
    class WML_ITEM Link
    {
    public:
        Link (Object* pkObject);

        void SetObject (Object* pkObject);
        Object* GetObject ();

        int GetQuantity () const;
        Object* GetLinkID ();

        void Add (Object* pkLinkID);

    protected:
        Object* m_pkObject;
        int m_iCurrent;
        std::vector<Object*> m_akLinkID;
    };

    // load/save support
    int BufferSize () const;
    int& BufferNext ();
    char* Buffer ();
    void Read (char*& racString);
    void Write (const char* acString);

    Object* GetFromMap (Object* pkLinkID);

    //*** END INTERNAL USE ONLY ***

protected:
    // base level streaming access
    friend class Object;
    bool InsertInMap (Object* pkKey, void* pvValue);
    void InsertInOrdered (Object* pkObject);

    // version of last loaded file
    Version m_kVersion;

    // top level object storage
    std::vector<Object*> m_apkTopLevel;

    // registration of objects on Save
    std::map<Object*,void*> m_kMap;

    // For saving objects in depth-first order.  If instead the objects are
    // saved based on hash-table order (in m_kMap), then the order of objects
    // for a scene graph can change between different runs of a program since
    // the memory addresses for the objects can change between runs.
    std::vector<Object*> m_kOrdered;

    // read/write always applied to buffer in memory
    int m_iBufferSize, m_iBufferNext;
    char* m_acBuffer;

    static const char ms_acTopLevel[];
};

#include "WmlStream.mcr"
#include "WmlStream.inl"

}

#endif


