// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

//----------------------------------------------------------------------------
template <class T>
Pointer<T>::Pointer (T* pkObject)
{
    m_pkObject = pkObject;
    if ( m_pkObject )
        m_pkObject->IncrementReferences();
}
//----------------------------------------------------------------------------
template <class T>
Pointer<T>::Pointer (const Pointer& rkPointer)
{
    m_pkObject = rkPointer.m_pkObject;
    if ( m_pkObject )
        m_pkObject->IncrementReferences();
}
//----------------------------------------------------------------------------
template <class T>
Pointer<T>::~Pointer ()
{
    if ( m_pkObject )
        m_pkObject->DecrementReferences();
}
//----------------------------------------------------------------------------
template <class T>
Pointer<T>::operator T* () const
{
    return m_pkObject;
}
//----------------------------------------------------------------------------
template <class T>
T& Pointer<T>::operator* () const
{
    return *m_pkObject;
}
//----------------------------------------------------------------------------
template <class T>
T* Pointer<T>::operator-> () const
{
    return m_pkObject;
}
//----------------------------------------------------------------------------
template <class T>
Pointer<T>& Pointer<T>::operator= (const Pointer& rkPointer)
{
    if ( m_pkObject != rkPointer.m_pkObject )
    {
        // The original code was
        //   if ( m_pkObject ) m_pkObject->DecrementReferences();
        //   m_pkObject = rkPoint.m_pkObject;
        //   if ( m_pkObject ) m_pkObject->IncrementReferences();
        // The following situation is not properly handled.
        //   NodePtr spkNode = new Node;      // N.ref = 1
        //   Node* pkChild = new Node;        // C.ref = 0
        //   spkNode->AttachChild(pkChild);   // C.ref = 1
        //   spkNode = pkChild;
        //       N.decref;  // N.ref = 0, N is destroyed and takes C with it
        //       N = C;     // C points to garbage, so N now does
        //       N.incref;  // modifies garbage memory
        // To avoid the side effect of indirectly deleting the input object,
        // rkPointer.m_pkObject must have its reference count incremented
        // before m_pkObject has its reference count decremented.
        // In the same example,
        //   spkNode = pkChild;
        //       C.incref;  // C.ref = 2
        //       N.decref;  // N.ref = 0, N is destroyed, C.ref = 1
        //       N = C;     // N.ref = 1

        if ( rkPointer.m_pkObject )
            rkPointer.m_pkObject->IncrementReferences();

        if ( m_pkObject )
            m_pkObject->DecrementReferences();

        m_pkObject = rkPointer.m_pkObject;
    }
    return *this;
}
//----------------------------------------------------------------------------
template <class T>
Pointer<T>& Pointer<T>::operator= (T* pkObject)
{
    // Read the comments in operator= (const Pointer&).
    if ( m_pkObject != pkObject )
    {
        if ( pkObject )
            pkObject->IncrementReferences();

        if ( m_pkObject )
            m_pkObject->DecrementReferences();

        m_pkObject = pkObject;
    }
    return *this;
}
//----------------------------------------------------------------------------
template <class T>
bool Pointer<T>::operator== (T* pkObject) const
{
    return ( m_pkObject == pkObject );
}
//----------------------------------------------------------------------------
template <class T>
bool Pointer<T>::operator!= (T* pkObject) const
{
    return ( m_pkObject != pkObject );
}
//----------------------------------------------------------------------------
template <class T>
bool Pointer<T>::operator== (const Pointer& rkPointer) const
{
    return ( m_pkObject == rkPointer.m_pkObject );
}
//----------------------------------------------------------------------------
template <class T>
bool Pointer<T>::operator!= (const Pointer& rkPointer) const
{
    return ( m_pkObject != rkPointer.m_pkObject );
}
//----------------------------------------------------------------------------
