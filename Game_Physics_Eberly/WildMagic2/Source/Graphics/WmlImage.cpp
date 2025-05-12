// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlImage.h"
using namespace Wml;
using namespace std;

WmlImplementRTTI(Image,Object);
WmlImplementStream(Image);

int Image::ms_aiBytesPerPixel[Image::IT_QUANTITY] =
{
    2, 3, 2, 4
};

// support for sharing images
map<string,Image*>* Image::ms_pkImages = NULL;

namespace Wml {
class _ImageInitTerm
{
public:
    _ImageInitTerm ()
    {
        Image::ms_pkImages = new map<string,Image*>;
    }

    ~_ImageInitTerm ()
    {
        Image::ms_pkImages->clear();
        delete Image::ms_pkImages;
        Image::ms_pkImages = NULL;
    }
};
}

static _ImageInitTerm _forceWmlImageInitTerm;

//----------------------------------------------------------------------------
Image::Image (Type eType, int iWidth, int iHeight, unsigned char* aucData,
    const char* acImageName, bool bRequirePowerOfTwo)
{
    if ( bRequirePowerOfTwo )
    {
        assert( System::IsPowerOfTwo(iWidth)
            &&  System::IsPowerOfTwo(iHeight) );
    }

    m_eType = eType;
    m_iWidth = iWidth;
    m_iHeight = iHeight;
    m_iQuantity = iWidth*iHeight;
    m_aucData = aucData;

    if ( acImageName )
    {
        SetName(acImageName);
    }
    else
    {
        // Generate the unique image name.
        char acFilename[32];
        sprintf(acFilename,"image%u.mif",m_uiID);
        SetName(acFilename);
    }
}
//----------------------------------------------------------------------------
Image::Image ()
{
    SetDefault();
}
//----------------------------------------------------------------------------
Image::~Image ()
{
    // remove image from map
    SetName(0);

    delete[] m_aucData;
}
//----------------------------------------------------------------------------
void Image::SetDefault ()
{
    m_eType = IT_QUANTITY;
    m_iWidth = 0;
    m_iHeight = 0;
    m_iQuantity = 0;
    m_aucData = NULL;
}
//----------------------------------------------------------------------------
void Image::SetName (const char* acFilename)
{
    if ( m_acName != acFilename )  // yes, this is intended to be ptr compare
    {
        if ( m_acName )
        {
            // Image is in the map, remove the map entry.
            RemoveAt(m_acName);
        }

        if ( acFilename )
        {
            // Add image to map with filename.
            Object::SetName(acFilename);
            SetAt(acFilename,this);
        }
    }
}
//----------------------------------------------------------------------------
Image* Image::Load (const char* acFilename, bool bRequirePowerOfTwo)
{
    if ( !acFilename )
        return 0;

    // Determine if image is already in memory and can be shared.
    Image* pkImage = GetAt(acFilename);
    if ( pkImage )
        return pkImage;

    FILE* pkFile = fopen(acFilename,"rb");
    if ( !pkFile )
        return 0;

    int iType, iWidth, iHeight;
    int iScanned = fscanf(pkFile,"Magic3D Image File#%d#%d#%d#",&iType,
        &iWidth,&iHeight);

    if ( iScanned == EOF
    ||   iScanned != 3
    ||   iType >= IT_QUANTITY
    ||   iWidth == 0
    ||   iHeight == 0 )
    {
        fclose(pkFile);
        return 0;
    }

    Type eType = (Type) iType;
    int iQuantity = iWidth*iHeight;
    int iSize = ms_aiBytesPerPixel[eType]*iQuantity;
    unsigned char* aucData = new unsigned char[iSize];
    int iRead = (int)fread(aucData,sizeof(unsigned char),iSize,pkFile);

    if ( iRead != iSize )
    {
        delete[] aucData;
        fclose(pkFile);
        return 0;
    }

    fclose(pkFile);

    if ( bRequirePowerOfTwo )
    {
        if ( !System::IsPowerOfTwo(iWidth)
        ||   !System::IsPowerOfTwo(iHeight) )
        {
            pkFile = fopen("ImageNotPowerOfTwo.txt","wt");
            assert( pkFile );
            fprintf(pkFile,"An image dimension is not power of two.");
            fprintf(pkFile,"  Width = %d  Height = %d\n",iWidth,iHeight);
            fclose(pkFile);
        }
    }

    return new Image(eType,iWidth,iHeight,aucData,acFilename,
        bRequirePowerOfTwo);
}
//----------------------------------------------------------------------------
bool Image::Save (const char* acFilename)
{
    if ( !acFilename )
        return false;

    FILE* pkFile = fopen(acFilename,"wb");
    if ( !pkFile )
        return false;

    // write header
    char acHeader[64];
    sprintf(acHeader,"Magic3D Image File#%d#%d#%d#",(int)m_eType,m_iWidth,
        m_iHeight);
    fwrite(acHeader,sizeof(char),strlen(acHeader),pkFile);

    // write data
    int iSize = ms_aiBytesPerPixel[m_eType]*m_iQuantity;
    fwrite(m_aucData,sizeof(unsigned char),iSize,pkFile);

    fclose(pkFile);
    return true;
}
//----------------------------------------------------------------------------
bool Image::SetAt (const char* acName, Image* pkImage)
{
    if ( ms_pkImages )
    {
        pair<map<string,Image*>::iterator,bool> kResult =
            ms_pkImages->insert(make_pair(string(acName),pkImage));
        return kResult.second;
    }

    return false;
}
//----------------------------------------------------------------------------
Image* Image::GetAt (const char* acName)
{
    if ( ms_pkImages )
    {
        map<string,Image*>::iterator kIter =
            ms_pkImages->find(string(acName));
        if ( kIter != ms_pkImages->end() )
            return kIter->second;
    }

    return NULL;
}
//----------------------------------------------------------------------------
bool Image::RemoveAt (const char* acName)
{
    if ( ms_pkImages )
    {
        ms_pkImages->erase(string(acName));
        return true;
    }

    return false;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* Image::Factory (Stream& rkStream)
{
    // NOTE.  The structure of this factory is slightly different than the
    // other classes.  The difference allows sharing of images.  Currently
    // Image is top-level (no classes derived from it).  Image has no
    // Object-derived members, so pkLink normally would have one element,
    // the object itself.  If the to-be-loaded image already exists in the
    // image map, then pkLink has that image object replace pkObject.

    Image* pkObject = new Image;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    if ( pkObject != pkLink->GetObject() )
    {
        // Image is already in memory, just share it.
        delete pkObject;
        pkObject = (Image*) pkLink->GetObject();
    }
    return pkObject;
}
//----------------------------------------------------------------------------
void Image::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Load(rkStream,pkLink);

    if ( GetName() )
    {
        if ( System::FileExists(GetName()) )
        {
            // File exists.  Read the image from it.
            Image* pkImage = Load(GetName());
            assert( pkImage );  // If fails, perhaps no read permission?
            pkLink->SetObject(pkImage);
        }
        else
        {
            // File does not exist.  Rather than assert, I dump the error
            // to a log file.  A likely problem is that GetName() is only a
            // partial pathname and the application has changed the working
            // directory.
            FILE* pkLog;
            if ( System::FileExists("ImageLoadFailed.txt") )
            {
                // Log file was created earlier, just dump the error."
                pkLog = fopen("ImageLoadFailed.txt","at");
                fprintf(pkLog,"%s\n",GetName());
            }
            else
            {
                pkLog = fopen("ImageLoadFailed.txt","wt");
                assert( pkLog );
                fprintf(pkLog,"Image loading failed.  A likely problem ");
                fprintf(pkLog,"is that the image name is only a\n");
                fprintf(pkLog,"partial pathname and the application has ");
                fprintf(pkLog,"changed the working directory.\n");
                fprintf(pkLog,"Failed loads:\n\n");
                fprintf(pkLog,"%s\n",GetName());
            }
            fclose(pkLog);

            SetDefault();
        }
        return;
    }

    // native data
    StreamReadEnum(rkStream,m_eType);
    StreamRead(rkStream,m_iWidth);
    StreamRead(rkStream,m_iHeight);
    StreamRead(rkStream,m_iQuantity);

    StreamRead(rkStream,m_aucData);
    if ( m_aucData )
    {
        int iBytes = ms_aiBytesPerPixel[m_eType]*m_iWidth*m_iHeight;
        m_aucData = new unsigned char[iBytes];
        StreamRead(rkStream,m_aucData,iBytes);
    }
}
//----------------------------------------------------------------------------
void Image::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool Image::Register (Stream& rkStream)
{
    return Object::Register(rkStream);
}
//----------------------------------------------------------------------------
void Image::Save (Stream& rkStream)
{
    Object::Save(rkStream);

    // If the image was loaded from a file, the filename was stored in the
    // Object name field.  If the name field was set explicitly, then
    if ( GetName() )
    {
        if ( !System::FileExists(GetName()) )
        {
            // File does not exist.  Write the image to it.
            bool bSaved = Save(GetName());
            assert( bSaved );  // If fails, perhaps no write permission?
        }
        // else:  File exists, assume it is this image.
        return;
    }

    // native data
    StreamWriteEnum(rkStream,m_eType);
    StreamWrite(rkStream,m_iWidth);
    StreamWrite(rkStream,m_iHeight);
    StreamWrite(rkStream,m_iQuantity);

    StreamWrite(rkStream,m_aucData);
    if ( m_aucData )
    {
        int iBytes = ms_aiBytesPerPixel[m_eType]*m_iWidth*m_iHeight;
        StreamWrite(rkStream,m_aucData,iBytes);
    }
}
//----------------------------------------------------------------------------
StringTree* Image::SaveStrings ()
{
    StringTree* pkTree = new StringTree(4,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));

    switch ( m_eType )
    {
    case IT_RGBA4444:
        pkTree->SetString(1,MakeString("type = RGBA4444"));
        break;
    case IT_RGB888:
        pkTree->SetString(1,MakeString("type = RGB888"));
        break;
    case IT_RGBA5551:
        pkTree->SetString(1,MakeString("type = RGBA5551"));
        break;
    case IT_RGBA8888:
        pkTree->SetString(1,MakeString("type = RGBA8888"));
        break;
    default:  // IT_QUANTITY
        break;
    }

    pkTree->SetString(2,MakeString("width =",m_iWidth));
    pkTree->SetString(3,MakeString("height =",m_iHeight));

    // children
    pkTree->SetChild(0,Object::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int Image::GetMemoryUsed () const
{
    int iBaseSize = sizeof(Image) - sizeof(Object);
    int iDynaSize = m_iQuantity*sizeof(m_aucData[0]);
    int iTotalSize = iBaseSize + iDynaSize + Object::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int Image::GetDiskUsed () const
{
    int iSize = Object::GetDiskUsed();

    if ( GetName() )
    {
        // image is stored externally from the scene graph
        return iSize;
    }

    iSize += StreamBytesEnum(m_eType) +
        sizeof(m_iWidth) +
        sizeof(m_iHeight) +
        sizeof(m_iQuantity);

    iSize += sizeof(m_aucData);
    if ( m_aucData )
    {
        int iBytes = ms_aiBytesPerPixel[m_eType]*m_iWidth*m_iHeight;
        iSize += iBytes*sizeof(m_aucData[0]);
    }

    return iSize;
}
//----------------------------------------------------------------------------
