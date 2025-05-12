// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlStringTree.h"
using namespace Wml;

//----------------------------------------------------------------------------
StringTree::StringTree (int iStringQuantity, int iStringGrowBy,
    int iChildQuantity, int iChildGrowBy)
    :
    m_kStrings(iStringQuantity),
    m_kChildren(iChildQuantity)
{
    m_iStringGrowBy = iStringGrowBy;
    m_iChildGrowBy = iChildGrowBy;

    int i;
    for (i = 0; i < (int)m_kStrings.size(); i++)
        m_kStrings[i] = NULL;

    for (i = 0; i < (int)m_kChildren.size(); i++)
        m_kChildren[i] = NULL;
}
//----------------------------------------------------------------------------
StringTree::~StringTree ()
{
    int i;
    for (i = 0; i < (int)m_kStrings.size(); i++)
        delete[] m_kStrings[i];

    for (i = 0; i < (int)m_kChildren.size(); i++)
        delete m_kChildren[i];
}
//----------------------------------------------------------------------------
char* StringTree::SetString (int i, char* acString)
{
    // increase storage to accommodate specified index
    if ( i >= (int)m_kStrings.size() )
    {
        SetStringQuantity(i + m_iStringGrowBy);
        for (int j = i; j < (int)m_kStrings.size(); j++)
            m_kStrings[j] = NULL;
    }

    char* acOldString = m_kStrings[i];
    m_kStrings[i] = acString;
    return acOldString;
}
//----------------------------------------------------------------------------
StringTree* StringTree::SetChild (int i, StringTree* pkChild)
{
    // increase storage to accommodate specified index
    if ( i >= (int)m_kChildren.size() )
    {
        SetChildQuantity(i + m_iChildGrowBy);
        for (int j = i; j < (int)m_kChildren.size(); j++)
            m_kChildren[j] = NULL;
    }

    StringTree* pkOldChild = m_kChildren[i];
    m_kChildren[i] = pkChild;
    return pkOldChild;
}
//----------------------------------------------------------------------------
bool StringTree::Save (const char* acFilename, int iTabSize)
{
    FILE* pkFile = fopen(acFilename,"wt");
    if ( !pkFile )
        return false;

    Save(pkFile,0,iTabSize);

    return fclose(pkFile) == 0;
}
//----------------------------------------------------------------------------
void StringTree::Save (FILE* pkFile, int iLevel, int iTabSize)
{
    // indent to proper location
    int i, iIndent = iLevel*iTabSize;
    if ( iIndent > 0 )
    {
        for (i = 0; i < iIndent; i++)
            fprintf(pkFile,"%c",' ');
    }

    // label with level
    if ( iLevel < 10 )
        fprintf(pkFile,"%d:  ",iLevel);
    else
        fprintf(pkFile,"%d: ",iLevel);

    // header string
    if ( m_kStrings.size() > 0 )
        fprintf(pkFile,"%s\n",m_kStrings[0]);
    else
        fprintf(pkFile,"<no header>\n");

    // body strings
    iIndent += 4;

    int j;
    for (j = 1; j < (int)m_kStrings.size(); j++)
    {
        for (i = 0; i < iIndent; i++)
            fprintf(pkFile,"%c",' ');
        fprintf(pkFile,"%s\n",m_kStrings[j]);
    }

    iLevel++;
    for (j = 0; j < (int)m_kChildren.size(); j++)
        m_kChildren[j]->Save(pkFile,iLevel,iTabSize);
}
//----------------------------------------------------------------------------
static void FormatFloat (float fValue, char* acString)
{
    if ( fValue > -FLT_MAX )
    {
        if ( fValue < FLT_MAX )
            sprintf(acString,"%f",fValue);
        else
            strcpy(acString,"INFINITY");
    }
    else
    {
        strcpy(acString,"-INFINITY");
    }
}
//----------------------------------------------------------------------------
static void FormatDouble (double dValue, char* acString)
{
    if ( dValue > -DBL_MAX )
    {
        if ( dValue < DBL_MAX )
            sprintf(acString,"%lf",dValue);
        else
            strcpy(acString,"INFINITY");
    }
    else
    {
        strcpy(acString,"-INFINITY");
    }
}
//----------------------------------------------------------------------------
char* Wml::MakeString (const RTTI* pkRTTI, const char* acName)
{
    assert( pkRTTI );
    const char* acRTTIName = pkRTTI->GetName();

    char* acString;
    if ( acName )
    {
        acString = new char[strlen(acRTTIName)+strlen(acName)+4];
        sprintf(acString,"%s <%s>",acRTTIName,acName);
    }
    else
    {
        acString = new char[strlen(acRTTIName)+2];
        sprintf(acString,"%s",acRTTIName);
    }
    return acString;
}
//----------------------------------------------------------------------------
char* Wml::MakeString (const char* acString)
{
    assert( acString );
    char* acDuplicate = new char[strlen(acString)+1];
    strcpy(acDuplicate,acString);
    return acDuplicate;
}
//----------------------------------------------------------------------------
char* Wml::MakeString (const char* acPrefix, bool bValue)
{
    assert( acPrefix );
    int iLength = (int)strlen(acPrefix)+2 + ( bValue ? 4 : 5 );
    char* acString = new char[iLength];
    if ( bValue )
        sprintf(acString,"%s true",acPrefix);
    else
        sprintf(acString,"%s false",acPrefix);
    return acString;
}
//----------------------------------------------------------------------------
char* Wml::MakeString (const char* acPrefix, char cValue)
{
    assert( acPrefix );
    int iLength = (int)strlen(acPrefix)+2 + 1;
    char* acString = new char[iLength];
    sprintf(acString,"%s %c",acPrefix,cValue);
    return acString;
}
//----------------------------------------------------------------------------
char* Wml::MakeString (const char* acPrefix, unsigned char ucValue)
{
    assert( acPrefix );
    char acDummy[32];
    sprintf(acDummy,"%u",(unsigned int)ucValue);
    int iLength = (int)strlen(acPrefix)+2 + (int)strlen(acDummy);
    char* acString = new char[iLength];
    sprintf(acString,"%s %s",acPrefix,acDummy);
    return acString;
}
//----------------------------------------------------------------------------
char* Wml::MakeString (const char* acPrefix, short sValue)
{
    assert( acPrefix );
    char acDummy[32];
    sprintf(acDummy,"%hd",sValue);
    int iLength = (int)strlen(acPrefix)+2 + (int)strlen(acDummy);
    char* acString = new char[iLength];
    sprintf(acString,"%s %s",acPrefix,acDummy);
    return acString;
}
//----------------------------------------------------------------------------
char* Wml::MakeString (const char* acPrefix, unsigned short usValue)
{
    assert( acPrefix );
    char acDummy[32];
    sprintf(acDummy,"%hu",usValue);
    int iLength = (int)strlen(acPrefix)+2 + (int)strlen(acDummy);
    char* acString = new char[iLength];
    sprintf(acString,"%s %s",acPrefix,acDummy);
    return acString;
}
//----------------------------------------------------------------------------
char* Wml::MakeString (const char* acPrefix, int iValue)
{
    assert( acPrefix );
    char acDummy[32];
    sprintf(acDummy,"%d",iValue);
    int iLength = (int)strlen(acPrefix)+2 + (int)strlen(acDummy);
    char* acString = new char[iLength];
    sprintf(acString,"%s %s",acPrefix,acDummy);
    return acString;
}
//----------------------------------------------------------------------------
char* Wml::MakeString (const char* acPrefix, unsigned int uiValue)
{
    assert( acPrefix );
    char acDummy[32];
    sprintf(acDummy,"%u",uiValue);
    int iLength = (int)strlen(acPrefix)+2 + (int)strlen(acDummy);
    char* acString = new char[iLength];
    sprintf(acString,"%s %s",acPrefix,acDummy);
    return acString;
}
//----------------------------------------------------------------------------
char* Wml::MakeString (const char* acPrefix, long lValue)
{
    assert( acPrefix );
    char acDummy[32];
    sprintf(acDummy,"%ld",lValue);
    int iLength = (int)strlen(acPrefix)+2 + (int)strlen(acDummy);
    char* acString = new char[iLength];
    sprintf(acString,"%s %s",acPrefix,acDummy);
    return acString;
}
//----------------------------------------------------------------------------
char* Wml::MakeString (const char* acPrefix, unsigned long ulValue)
{
    assert( acPrefix );
    char acDummy[32];
    sprintf(acDummy,"%lu",ulValue);
    int iLength = (int)strlen(acPrefix)+2 + (int)strlen(acDummy);
    char* acString = new char[iLength];
    sprintf(acString,"%s %s",acPrefix,acDummy);
    return acString;
}
//----------------------------------------------------------------------------
char* Wml::MakeString (const char* acPrefix, float fValue)
{
    assert( acPrefix );
    char acDummy[256];
    FormatFloat(fValue,acDummy);

    int iLength = (int)strlen(acPrefix)+2 + (int)strlen(acDummy);
    char* acString = new char[iLength];
    sprintf(acString,"%s %s",acPrefix,acDummy);
    return acString;
}
//----------------------------------------------------------------------------
char* Wml::MakeString (const char* acPrefix, double dValue)
{
    assert( acPrefix );
    char acDummy[256];
    FormatDouble(dValue,acDummy);

    int iLength = (int)strlen(acPrefix)+2 + (int)strlen(acDummy);
    char* acString = new char[iLength];
    sprintf(acString,"%s %s",acPrefix,acDummy);
    return acString;
}
//----------------------------------------------------------------------------
char* Wml::MakeString (const char* acPrefix, void* pvValue)
{
    assert( acPrefix );
    char acDummy[32];
    sprintf(acDummy,"%p",pvValue);
    int iLength = (int)strlen(acPrefix)+2 + (int)strlen(acDummy);
    char* acString = new char[iLength];
    sprintf(acString,"%s %s",acPrefix,acDummy);
    return acString;
}
//----------------------------------------------------------------------------
char* Wml::MakeString (const char* acPrefix, const char* acValue)
{
    assert( acPrefix && acValue );
    int iLength = (int)strlen(acPrefix)+2 + (int)strlen(acValue);
    char* acString = new char[iLength];
    sprintf(acString,"%s %s",acPrefix,acValue);
    return acString;
}
//----------------------------------------------------------------------------
char* Wml::MakeString (const char* acPrefix, const ColorRGB& rkValue)
{
    assert( acPrefix );
    char acDummy[256];
    sprintf(acDummy,"(r: %f, g: %f, b: %f)",rkValue.r,rkValue.g,rkValue.b);
    int iLength = (int)strlen(acPrefix)+2 + (int)strlen(acDummy);
    char* acString = new char[iLength];
    sprintf(acString,"%s %s",acPrefix,acDummy);
    return acString;
}
//----------------------------------------------------------------------------
char* Wml::MakeString (const char* acPrefix, const Matrix3f& rkValue)
{
    assert( acPrefix );
    char acDummy[256];
    sprintf(acDummy,
    "[c0:(%.3f,%.3f,%.3f),c1:(%.3f,%.3f,%.3f),c2:(%.3f,%.3f,%.3f)",
        rkValue[0][0],rkValue[1][0],rkValue[2][0],
        rkValue[0][1],rkValue[1][1],rkValue[2][1],
        rkValue[0][2],rkValue[1][2],rkValue[2][2]);
    int iLength = (int)strlen(acPrefix)+2 + (int)strlen(acDummy);
    char* acString = new char[iLength];
    sprintf(acString,"%s %s",acPrefix,acDummy);
    return acString;
}
//----------------------------------------------------------------------------
char* Wml::MakeString (const char* acPrefix, const Quaternionf& rkValue)
{
    assert( acPrefix );
    char acDummy[256];
    sprintf(acDummy,"(w: %.3f, x: %.3f, y: %.3f, z: %.3f)",rkValue.W(),
        rkValue.X(),rkValue.Y(),rkValue.Z());
    int iLength = (int)strlen(acPrefix)+2 + (int)strlen(acDummy);
    char* acString = new char[iLength];
    sprintf(acString,"%s %s",acPrefix,acDummy);
    return acString;
}
//----------------------------------------------------------------------------
char* Wml::MakeString (const char* acPrefix, const Vector2f& rkValue)
{
    assert( acPrefix );
    char acX[256], acY[256];
    FormatFloat(rkValue.X(),acX);
    FormatFloat(rkValue.Y(),acY);

    char acDummy[256];
    sprintf(acDummy,"(x: %s, y: %s)",acX,acY);
    int iLength = (int)strlen(acPrefix)+2 + (int)strlen(acDummy);
    char* acString = new char[iLength];
    sprintf(acString,"%s %s",acPrefix,acDummy);
    return acString;
}
//----------------------------------------------------------------------------
char* Wml::MakeString (const char* acPrefix, const Vector3f& rkValue)
{
    assert( acPrefix );
    char acX[256], acY[256], acZ[256];
    FormatFloat(rkValue.X(),acX);
    FormatFloat(rkValue.Y(),acY);
    FormatFloat(rkValue.Z(),acZ);

    char acDummy[256];
    sprintf(acDummy,"(x: %s, y: %s, z: %s)",acX,acY,acZ);
    int iLength = (int)strlen(acPrefix)+2 + (int)strlen(acDummy);
    char* acString = new char[iLength];
    sprintf(acString,"%s %s",acPrefix,acDummy);
    return acString;
}
//----------------------------------------------------------------------------
char* Wml::MakeString (const char* acPrefix, const Bound& rkValue)
{
    assert( acPrefix );
    char acX[256], acY[256], acZ[256], acR[256];
    FormatFloat(rkValue.Center().X(),acX);
    FormatFloat(rkValue.Center().Y(),acY);
    FormatFloat(rkValue.Center().Z(),acZ);
    FormatFloat(rkValue.Radius(),acR);

    char acDummy[256];
    sprintf(acDummy,"(x: %s, y: %s, z: %s, r: %s)",acX,acY,acZ,acR);
    int iLength = (int)strlen(acPrefix)+2 + (int)strlen(acDummy);
    char* acString = new char[iLength];
    sprintf(acString,"%s %s",acPrefix,acDummy);
    return acString;
}
//----------------------------------------------------------------------------
