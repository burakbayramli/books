// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLIMAGE_H
#define WMLIMAGE_H

// IMAGE FILES
//
// A Magic Image File is a simple file type that is designed to avoid having
// the engine provide a generic file converter library.  The file header is a
// non-null-terminated ASCII string (starts with 'M', ends with '#'),
//     Magic3D Image File#t#w#h#
// where t is in {0,..,IT_QUANTITY-1} (the values of the enumerations in
// 'Type'), w is the unsigned integer width, and h is the unsigned integer
// height.  Immediately following the last # is the unsigned character data.
// There must be ms_auiBytesPerPixel[t]*w*h bytes of data.
//
// A few error conditions can arise.  On any error condition, the image
// members are set just as in the default constructor.  The conditions
// are:
//   1. file does not exist
//   2. file exists, but not a Magic image file
//   3. file is Magic image, but
//      a. t is not in {0,..,IT_QUANTITY-1}
//      b. w is zero
//      c. h is zero
//      d. the byte quantity for the data is incorrect
//
// I will use the extension ".mif" for Magic image files, but the load
// and save routines do not depend on this.

// IMAGE SHARING
//
// The Wml::Object::m_acName field is used by Wml::Image either to store the
// filename if the image is loaded from disk or a unique name if the image is
// created procedurally.  In the latter case, the name is generated as
// "imageN.mif" where N is the Wml::Object::m_uiID field.  If two images are
// loaded, both having the same filenames, then only one copy of the image
// will be in memory.  The two clients of the common image will share that
// image.  A to-be-saved image with the name field set will be saved in a
// MIF file that is separate from the MGC scene graph file.  If you want an
// image file to be saved in the MGC file, call SetName(0) on that image to
// remove the name.  That is, any image object with no filename will not be
// streamed separately and will not be shareable.

#include "WmlObject.h"
#include <map>

namespace Wml
{

class WML_ITEM Image : public Object
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    enum Type
    {
        IT_RGBA4444,
        IT_RGB888,
        IT_RGBA5551,
        IT_RGBA8888,
        IT_QUANTITY
    };

    // Construction and destruction.  WmlImage accepts responsibility for
    // deleting the input array.  The WmlObject::m_acName field is used as a
    // unique identifier for the image for purposes of sharing.  The caller of
    // the constructor may provided a name.  If not, the constructor generates
    // a unique name "imageN.mif" where N is the WmlObject::m_uiID field. A
    // global map of images is maintained for sharing purposes.
    Image (Type eType, int iWidth, int iHeight, unsigned char* aucData,
        const char* acImageName = 0, bool bRequirePowerOfTwo = true);
    virtual ~Image ();

    // member access
    Type GetType () const;
    int GetBytesPerPixel () const;
    int GetWidth () const;
    int GetHeight () const;
    int GetQuantity () const;
    unsigned char* GetData () const;
    unsigned char* operator() (int i);

    // Streaming support.  The sharing system is automatically invoked by
    // these calls.  In Load, if an image corresponding to the filename is
    // already in memory, then that image is returned (i.e. shared).
    // Otherwise, a new image is created and returned.  The filename is used
    // as the image name.
    static Image* Load (const char* acFilename,
        bool bRequirePowerOfTwo = true);

    // This is intended to support saving procedurally generated images or
    // for utilities that convert to MIF from another format.  The filename
    // in this call does not replace the image name that might already exist.
    bool Save (const char* acFilename);

    // Support for sharing images.  Note that WmlObject::SetName is not
    // virtual, so WmlImage::SetName hides it.  Do not circumvent this
    // function by explicitly calling WmlObject::SetName on an WmlImage
    // object.
    void SetName (const char* acName);

protected:
    // support for streaming
    Image ();
    void SetDefault ();

    Type m_eType;
    int m_iWidth, m_iHeight, m_iQuantity;
    unsigned char* m_aucData;

    static int ms_aiBytesPerPixel[IT_QUANTITY];

    // support for sharing images
    friend class _ImageInitTerm;
    static const unsigned int ms_uiMapSize;
    static std::map<std::string,Image*>* ms_pkImages;
    static bool SetAt (const char* acName, Image* pkImage);
    static Image* GetAt (const char* acName);
    static bool RemoveAt (const char* acName);
};

WmlSmartPointer(Image);
WmlRegisterStream(Image);
#include "WmlImage.inl"

}

#endif
