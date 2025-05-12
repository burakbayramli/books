// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "WmlLCPPolyDist.h"
using namespace Wml;
using namespace std;

//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
LCPPolyDist<VectorIn,TVector,TTuple>::LCPPolyDist (int iNumPoints1, 
    VectorIn* akP1, int iNumFaces1, TTuple* akF1, int iNumPoints2, 
    VectorIn* akP2, int iNumFaces2, TTuple* akF2, int& riStatusCode,
    float& rfDist, VectorIn akRes[/*2*/])
{
    // Vertices and faces entry point
    m_iDimension = sizeof(TVector)/sizeof(double);

    LOGFUNCTION(OpenLog());
    LOGFUNCTION(LogVerticesAndFaces(iNumPoints1,akP1,iNumFaces1,akF1));
    LOGFUNCTION(LogVerticesAndFaces(iNumPoints2,akP2,iNumFaces2,akF2));

    m_iNumPoints1 = iNumPoints1;
    m_akP1 = new TVector[iNumPoints1];
    int i, j;
    for (i = 0; i < iNumPoints1; i++)
    {
        for (j = 0; j < m_iDimension; j++)
            m_akP1[i][j] = (double) akP1[i][j];
    }
    m_iNumFaces1 = iNumFaces1;
    m_akF1 = akF1;
    m_iNumPoints2 = iNumPoints2;
    m_akP2 = new TVector[iNumPoints2];
    for (i = 0; i < iNumPoints2; i++)
    {
        for (j = 0; j < m_iDimension; j++)
            m_akP2[i][j] = (double) akP2[i][j];
    }
    m_iNumFaces2 = iNumFaces2;
    m_akF2 = akF2;

    m_iNumEquations = iNumFaces1+iNumFaces2+2*m_iDimension;

    m_adB1 = new double[m_iNumEquations];
    m_akA1 = new TVector[iNumFaces1];
    GenerateHalfSpaceDescription(m_iNumPoints1,m_akP1,m_iNumFaces1,m_akF1,
        m_adB1,m_akA1);

    m_adB2 = new double[m_iNumEquations];
    m_akA2 = new TVector[iNumFaces2];
    GenerateHalfSpaceDescription(m_iNumPoints2,m_akP2,m_iNumFaces2,m_akF2,
        m_adB2,m_akA2);

    rfDist = (float) ProcessLoop(false,riStatusCode,akRes);

    delete[] m_akA1;
    delete[] m_adB1;
    delete[] m_akA2;
    delete[] m_adB2;
    delete[] m_akP1;
    delete[] m_akP2;

    LOGFUNCTION(CloseLog ());
}
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
LCPPolyDist<VectorIn,TVector,TTuple>::LCPPolyDist (int iNum1, float* afB1,
    VectorIn* akA1, int iNum2, float* afB2, VectorIn* akA2, int& riStatusCode,
    float& rfDist, VectorIn akRes[/*2*/])
{
    // Halfspaces entry point
    m_iDimension = sizeof(TVector)/sizeof(double);

    m_iNumEquations = iNum1+iNum2+2*m_iDimension;
    m_iNumFaces1 = iNum1;
    m_iNumFaces2 = iNum2;

    m_adB1 = new double[iNum1];
    m_akA1 = new TVector[iNum1];
    int i, j;
    for (i = 0; i < iNum1; i++)
    {
        m_adB1[i] = (double) afB1[i];
        for (j = 0; j < m_iDimension; j++)
            m_akA1[i][j] = (double) akA1[i][j];
    }
    m_adB2 = new double[iNum1];
    m_akA2 = new TVector[iNum1];
    for (i = 0; i < iNum2; i++)
    {
        m_adB2[i] = (double) afB2[i];
        for (j = 0; j < m_iDimension; j++)
            m_akA2[i][j] = (double) akA2[i][j];
    }

    LOGFUNCTION(OpenLog());
    LOGFUNCTION(LogHalfspaces(m_adB1,m_akA1,iNum1));
    LOGFUNCTION(LogHalfspaces(m_adB2,m_akA2,iNum2));

    LOGFUNCTION(RandomizeHalfspaces());
    
    rfDist = (float) ProcessLoop(true,riStatusCode,akRes);

    LOGFUNCTION(CloseLog());

    delete[] m_adB1;
    delete[] m_akA1;
    delete[] m_adB2;
    delete[] m_akA2;
}
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
double LCPPolyDist<VectorIn,TVector,TTuple>::ProcessLoop (bool bHS,
    int& riStatusCode, VectorIn* akRes)
{
    int i;
    double* adQ = new double[m_iNumEquations];;
    double** aadM = new double*[m_iNumEquations];
    for (i = 0; i < m_iNumEquations; i++)
        aadM[i] = new double[m_iNumEquations];

    riStatusCode = 0;

    bool bContinue = true;
    if ( !BuildMatrices(aadM,adQ) )
    {
        LOGFUNCTION(CloseLog());
        akRes[0] = VectorIn::ZERO;
        akRes[1] = VectorIn::ZERO;
        bContinue = false;
    }

    LOGFUNCTION(PrintMatrices(aadM,adQ));

    int iTryNumber = 0;
    float fDistance = -10.0f;
    while ( bContinue )
    {
        double* adZResult = new double[m_iNumEquations];
        double* adWResult = new double[m_iNumEquations];
        int iReturn;

        LCPSolver::LCPSolver(m_iNumEquations,aadM,adQ,adZResult,adWResult,
            iReturn);

        switch ( iReturn )
        {
        case SC_FOUND_TRIVIAL_SOLUTION:
        {
            riStatusCode = SC_FOUND_TRIVIAL_SOLUTION;
            break;
        }
        case SC_FOUND_SOLUTION:
        {
            // solution found
            TVector* akResD = new TVector[2];
            for (i = 0; i < m_iDimension; i++)
            {
                akResD[0][i] = (float) adZResult[i];
                akResD[1][i] = (float) adZResult[i+m_iDimension];
            }

            TVector kDiff = akResD[0]-akResD[1];
            fDistance = (float) kDiff.Length();

            riStatusCode = VerifySolution(akResD);

            if ( !bHS )
            {
                LOGFUNCTION(VerifyWithTestPoints(akResD,riStatusCode));
            }

            for (i = 0; i < m_iDimension; i++)
            {
                akRes[0][i] = (float) akResD[0][i];
                akRes[1][i] = (float) akResD[1][i];
            }
            delete[] akResD;
            bContinue = false;
            break;
        }
        case SC_CANNOT_REMOVE_COMPLEMENTARY:
        {
            LOGFUNCTION(LogRetries(iTryNumber))
            if ( iTryNumber == 3 )
            {
                riStatusCode = SC_CANNOT_REMOVE_COMPLEMENTARY;
                bContinue = false;
                break;
            }
            MoveHalfspaces(m_iNumFaces1,m_adB1,m_akA1);
            LOGFUNCTION(LogHalfspaces(m_adB1,m_akA1,m_iNumFaces1));
            MoveHalfspaces(m_iNumFaces2,m_adB2,m_akA2);
            LOGFUNCTION(LogHalfspaces(m_adB2,m_akA2,m_iNumFaces2));
            BuildMatrices(aadM,adQ);
            iTryNumber++;
            break;
        }
        case SC_EXCEEDED_MAX_RETRIES:
        {
            LOGFUNCTION(LCPSolverLoopLimit());
            riStatusCode = SC_EXCEEDED_MAX_RETRIES;
            bContinue = false;
            break;
        }
        }
        delete[] adWResult;
        delete[] adZResult;
    }
    for (i = 0; i < m_iNumEquations; i++)
        delete[] aadM[i];
    delete[] aadM;
    delete[] adQ;

    return fDistance;
}
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
bool LCPPolyDist<VectorIn,TVector,TTuple>::BuildMatrices (double** aadM,
    double* adQ)
{
    // zero aadM and adQ
    memset(adQ,0,m_iNumEquations*sizeof(double));
    int i;
    for (i = 0; i < m_iNumEquations; i++)
        memset(aadM[i],0,m_iNumEquations*sizeof(double));

    // enter pdZCoef terms
    // first matrix S
    int iTwoDimension = 2*m_iDimension;
    for (i = 0; i < iTwoDimension; i++)
    {
        aadM[i][i] = 2.0;
        if ( i < m_iDimension )
        {
            aadM[i][i+m_iDimension] = -2.0;
            aadM[i+m_iDimension][i] = -2.0;
        }
    }

    double dQMin = 1.0;
    int j, k;
    for (i = 0, j = iTwoDimension; i < m_iNumFaces1; i++, j++)
    {
        for (k = 0; k < m_iDimension; k++)
        {
            aadM[j][k] = -m_akA1[i][k];       // -A1
            aadM[k][j] = m_akA1[i][k];        // A1 transpose
        }

        adQ[j] = m_adB1[i];
        if ( adQ[j] < dQMin )
            dQMin = adQ[j];
    }

    for (i = 0, j = iTwoDimension+m_iNumFaces1; i < m_iNumFaces2; i++, j++)
    {
        int n;
        for (k = 0, n = m_iDimension; k < m_iDimension; k++, n++)
        {
            aadM[j][n] = -m_akA2[i][k];     // -A2
            aadM[n][j] = m_akA2[i][k];      // A2 transpose
        }

        adQ[j] = m_adB2[i];
        if ( adQ[j] < dQMin )
            dQMin = adQ[j];
    }
    if ( dQMin >= 0 )
        return false;

    return true;
}
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
void LCPPolyDist<VectorIn,TVector,TTuple>::ComputeHalfspaces (int iNumPoints,
    const TVector* akP, int iNumFaces, const TTuple* akF, TVector* akA,
    double* afB)
{
    int j, k;

    TVector kAvgPt = TVector::ZERO;
    for (j = 0; j < iNumPoints; j++)
    {
        for (k = 0; k < m_iDimension; k++)
            kAvgPt[k] += akP[j][k];
    }
    kAvgPt /= (double) iNumPoints;

    TVector kD1, kD2, kN;

    for (j = 0; j < iNumFaces; j++)
    {
        // NOTE.  Cross product in 2D and 3D is not easy to put into a
        // template class.  Vector2<double> has a Cross member function that
        // takes a dummy input vector that is ignored just to support this
        // functionality.
        kD1 = akP[akF[j][1]]-akP[akF[j][0]];
        if ( m_iDimension == 3 )
            kD2 = akP[akF[j][2]]-akP[akF[j][0]];
        kN = kD1.Cross(kD2);

        double fRHS = kN.Dot(akP[akF[j][1]]);

        double fMult = ( kN.Dot(kAvgPt) <= fRHS ? 1.0f : -1.0f );
        afB[j] = fMult*fRHS;
        //akA[j] = fMult*kN;
        for (k = 0; k < m_iDimension; k++)
            akA[j][k] = fMult*kN[k];
    }
}
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
void LCPPolyDist<VectorIn,TVector,TTuple>::GenerateHalfSpaceDescription
    (int iNumPoints, TVector* akP, int iNumFaces, TTuple* akF, double* afB,
    TVector* akA)
{
    LOGFUNCTION(RandomizeArray(iNumPoints,akP));
    LOGFUNCTION(LogVertices(akP,iNumPoints));

    ComputeHalfspaces(iNumPoints,akP,iNumFaces,akF,akA,afB);

    LOGFUNCTION(LogHalfspaces(afB,akA,iNumFaces));
}
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
void LCPPolyDist<VectorIn,TVector,TTuple>::ChangeFaceOrder (int iNumFaces, 
    TTuple* akF)
{
    TTuple kTemp = akF[0];
    for (int j = 1; j < iNumFaces; j++)
        akF[j-1] = akF[j];
    akF[iNumFaces-1] = kTemp;
}
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
void LCPPolyDist<VectorIn,TVector,TTuple>::MoveHalfspaces (int iNumFaces,
    double* afB, TVector* akA)
{
    TVector kTemp = akA[0];
    double fTemp = afB[0];
    for (int j = 1; j < iNumFaces; j++)
    {
        akA[j-1] = akA[j];
        afB[j-1] = afB[j];
    }
    akA[iNumFaces-1] = kTemp;
    afB[iNumFaces-1] = fTemp;
}
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
void LCPPolyDist<VectorIn,TVector,TTuple>::MovePoints ()
{ 
    ChangeFaceOrder(m_iNumFaces1,m_akF1);
    ChangeFaceOrder(m_iNumFaces2,m_akF2);
    GenerateHalfSpaceDescription(m_iNumPoints1,m_akP1,m_iNumFaces1,m_akF1,
        m_adB1,m_akA1);
    GenerateHalfSpaceDescription(m_iNumPoints2,m_akP2,m_iNumFaces2,m_akF2,
        m_adB2,m_akA2);
}
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
int LCPPolyDist<VectorIn,TVector,TTuple>::VerifySolution (
    const TVector* akRes)
{
    // check to see that the first solution point is in the first polyhedron
    // and the second in the second
    double fDiff;
    int i;
    int iReturnValue = SC_FOUND_SOLUTION;

    for (i = 0; i < m_iNumFaces1; i++)
    {
        fDiff = m_akA1[i].Dot(akRes[0])-m_adB1[i];
        if ( fDiff > VERIFY_MIN_DIFFERENCE )
        {
            LOGFUNCTION(LogVerifyFailure(1,i,fDiff));
            iReturnValue = SC_VERIFY_FAILURE;
        }
    }

    for (i = 0; i < m_iNumFaces2; i++)
    {
        fDiff = m_akA2[i].Dot(akRes[1])-m_adB2[i];
        if ( fDiff > VERIFY_MIN_DIFFERENCE )
        {
            LOGFUNCTION(LogVerifyFailure(2,i,fDiff));
            iReturnValue = SC_VERIFY_FAILURE;
        }
    }
    return iReturnValue;
}
//----------------------------------------------------------------------------

#ifdef LCPPOLYDIST_LOG
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
void LCPPolyDist<VectorIn,TVector,TTuple>::OpenLog ()
{
    if ( !m_kLog.is_open() )
    {
        // open the log file
        m_kLog.open("LCPPolyDist.log");
        assert( m_kLog );
        if ( !m_kLog )
            return;
    }
    
    m_kLog << "LCP Polyhedron Distance Log" << endl;

    // print the current date and time
    time_t kClock;
    time(&kClock);
    m_kLog << "Time: " << asctime(localtime(&kClock)) << endl << endl;

    // use scientific notation for floating point output
    m_kLog.setf(ios::scientific,ios::floatfield);
}
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
void LCPPolyDist<VectorIn,TVector,TTuple>::CloseLog ()
{
    m_kLog.flush();
    m_kLog.close();
}
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
void LCPPolyDist<VectorIn,TVector,TTuple>::PrintMatrices(double** aadM,
    double* adQ)
{
    // print the input to the solver
    m_kLog << "There are " << m_iNumEquations
        << " rows and columns in this problem." << endl << endl;

    m_kLog << "The matrix M." << endl;
    int i;
    for (i = 0; i < m_iNumEquations; i++)
    {
        for (int j = 0; j < m_iNumEquations; j++)
            m_kLog << showpos << aadM[i][j] << ' ';
        m_kLog << endl;
    }

    m_kLog << "The vector Q." << endl;
    for (i = 0; i < m_iNumEquations; i++)
        m_kLog << showpos << adQ[i] << ' ';
    m_kLog << endl;
}
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
void LCPPolyDist<VectorIn,TVector,TTuple>::LogRetries (int iTryNumber)
{
    m_kLog << "LCPPolyDist cannot remove complementary variable. ";
    m_kLog << "No solution. iTryNumber = " << iTryNumber << endl;
}
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
void LCPPolyDist<VectorIn,TVector,TTuple>::LCPSolverLoopLimit ()
{
    m_kLog << "No solution found. LCPSolver used the maximum";
    m_kLog << " parameterized number of loops." << endl;
}
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
void LCPPolyDist<VectorIn,TVector,TTuple>::LogVertices (const TVector* akP,
    int iNum)
{
    // write triangle vertices to log
    m_kLog << endl << "Triangle vertices." << endl;
    for (int i = 0; i < iNum; i++)
    {
        m_kLog << "(" << akP[i][0];
        for (int j = 1; j < m_iDimension; j++)
            m_kLog << ", " << akP[i][j];
        m_kLog << ")" << endl;
    }
    m_kLog << endl;
}
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
void LCPPolyDist<VectorIn,TVector,TTuple>::LogVerticesAndFaces (
    int iNumPoints, const VectorIn* akP, int iNumFaces, const TTuple* akF)
{
    m_kLog << endl << "This polyhedron has " << iNumPoints; 
    m_kLog << " vertices." << endl << "They are:" << endl;
    int i, j;
    for (i = 0; i < iNumPoints; i++)
    {
        m_kLog << "(" << akP[i][0];
        for(j = 1; j < m_iDimension; j++)
            m_kLog << ", " << akP[i][j];
        m_kLog << ")" << endl;
    }
    m_kLog << " The number of faces is " << iNumFaces << "." << endl; 
    m_kLog << " And they are (counting the vertices above from 0 to ";
    m_kLog << iNumPoints-1 << ")" << endl;
    for (i = 0; i < iNumFaces; i++)
    {
        m_kLog << "(" << akF[i][0];
        for(j = 1; j < m_iDimension; j++)
            m_kLog << ", " << akF[i][j];
        m_kLog << ")" << endl;
    }
}
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
void LCPPolyDist<VectorIn,TVector,TTuple>::LogHalfspaces (const double* afB,
    const TVector* akP, int iNum)
{
    // write halfspaces generated by triangle vertices to log
    m_kLog << endl << "Halfspaces:" << endl;
    string sYZ[] = { "*y ", "*z " };
    for (int i = 0; i < iNum; i++)
    {
        m_kLog << akP[i][0] << "*x ";
        for(int j = 1; j < m_iDimension; j++)
            m_kLog << akP[i][j] << sYZ[j-1];
        m_kLog << "<= " << afB[i] << endl;
    }
    m_kLog << endl;
}
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
void LCPPolyDist<VectorIn,TVector,TTuple>::SetupTestPoints (int iIndex,
    int iNumFaces, int iNumPoints, TVector kZRes, const double* afB,
    const TVector* akA, const TVector* akP, TVector* akTestPoints)
{
    int i;
    TVector kSum = TVector::ZERO;

    for (i = 0; i < iNumPoints; i++)
        kSum += akP[i];

    akTestPoints[0] = kSum/((double) iNumPoints);

    int m = iNumPoints < 4 ? iNumPoints : 4;
    for (i = 0; i < 4; i++)
    {
        if ( i < m )
        {
            akTestPoints[i+1] = kZRes*0.5f+akP[i]*0.5f;
            akTestPoints[i+5] = kZRes*0.8f+akP[i]*0.2f;
        }
        else
        {
            akTestPoints[i+1] = kZRes;
            akTestPoints[i+5] = kZRes;
        }
    }

    double fAmtOut = 0.2f;
    akTestPoints[9] = (akTestPoints[1]+akTestPoints[5])*0.5f;
    akTestPoints[10] = kZRes*0.5f+akTestPoints[0]*0.5f;
    akTestPoints[11] = kZRes*0.8f+akTestPoints[0]*0.2f;
    akTestPoints[12] = kZRes*(1.0f+fAmtOut)-
        akTestPoints[0]*fAmtOut;        // should be outside polyhedron

    LogSolutionWithTestPoints(iIndex,kZRes,akTestPoints);

    // eliminate generated points outside poly
    for (i = 0; i < 13; i++)
    {
        for (int j = 0; j < iNumFaces; j++)
        {
            // For each point and each constraint calc constraint value
            double fDiff = akA[j].Dot(akTestPoints[i]) - afB[j];
            if ( fDiff > VERIFY_MIN_DIFFERENCE )        // if not met
                akTestPoints[i] = kZRes;      // replace with solution pt
        }
    }
    m_kLog << "Test points reduced for containment in polyhedron:" << endl;
    LogTestPoints(akTestPoints);
}
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
void LCPPolyDist<VectorIn,TVector,TTuple>::LogTestPoints (
    const TVector* akTestPoints)
{
    for (int i = 0; i < 13; i++) 
    {
        m_kLog << "(" << akTestPoints[i][0];
        for (int j = 1; j < m_iDimension; j++ )
            m_kLog << ", " << akTestPoints[i][j];
        m_kLog << ")" << endl;
    }
    m_kLog << endl;
}
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
void LCPPolyDist<VectorIn,TVector,TTuple>::LogSolutionWithTestPoints (
    int iIndex, TVector kZRes, const TVector* akTestPoints)
{
    // write generated test points to log
    if ( iIndex == 1 )
        m_kLog << endl << endl;
    m_kLog << "Solution point from polyhedron " << iIndex;
    m_kLog << " = (" << kZRes[0];
    for (int i = 1; i < m_iDimension; i++)
        m_kLog << ", " << kZRes[i];
    m_kLog << ")" << endl;
    m_kLog << "The generated test points are :" << endl;
    LogTestPoints(akTestPoints);
}       
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
void LCPPolyDist<VectorIn,TVector,TTuple>::VerifyWithTestPoints (
    const TVector* akRes, int& riStatusCode)
{
    TVector* akTestPoints1 = new TVector[13];
    TVector* akTestPoints2 = new TVector[13];

    SetupTestPoints(1,m_iNumFaces1,m_iNumPoints1,akRes[0],m_adB1,m_akA1,
        m_akP1,akTestPoints1);
    SetupTestPoints(2,m_iNumFaces2,m_iNumPoints2,akRes[1],m_adB2,m_akA2,
        m_akP2,akTestPoints2);

    double fDiffSq;
    double fMin;
    TVector kDiff;
    int iLine = 0;
    int jLine = 0;

    // min distance between generated points note that one of these points
    // is the "solution"
    int i;
    for (i = 0; i < 13; i++)
    {
        for (int j = 0; j < 13; j++)
        {
            kDiff = akTestPoints1[i]-akTestPoints2[j];
            fDiffSq = kDiff.Dot(kDiff);
            if ( i == 0 && j == 0 )
            {
                fMin = fDiffSq;
            }
            else
            {
                if ( fDiffSq < fMin )
                {
                    fMin = fDiffSq;
                    iLine = i;
                    jLine = j;
                }
            }
        }
    }

    kDiff = akRes[0]-akRes[1];
    float fDistance = (float) kDiff.Dot(kDiff);

    riStatusCode = SC_FOUND_SOLUTION;
    if ( fDistance > fDiffSq )
    {
        m_kLog << "Test points closer than solution points by ";
        m_kLog << fDistance-fDiffSq << " squared units.\n";
        if ( (fDistance-fDiffSq > VERIFY_MIN_DIFFERENCE)
        &&   (iLine != 12 || jLine != 12) )
        {
            riStatusCode = SC_TEST_POINTS_TEST_FAILED;
        }
    }
    m_kLog << endl << " Solution points are separated by "
        << Mathf::Sqrt(fDistance);
    m_kLog << " units." << endl;
    m_kLog << "The smallest distance between test points is "
        << Mathf::Sqrt((float)fMin);
    m_kLog << endl << "and occurs for (" << akTestPoints1[iLine][0];
    for (i = 1; i < m_iDimension; i++)
    {
        m_kLog << ", " << akTestPoints1[iLine][i];
    }
    m_kLog << ") and (" << akTestPoints2[jLine][0];
    for (i = 1; i < m_iDimension; i++)
        m_kLog << ", " << akTestPoints2[jLine][i];
    m_kLog << ")" << endl;

    delete[] akTestPoints1;
    delete[] akTestPoints2;
}
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
void LCPPolyDist<VectorIn,TVector,TTuple>::RandomizeArray (int iNumPoints,
    TVector* akP)
{
    // large values (0.9) of RANDOM_WIDTH tolerated with sample data files 
    for (int j = 0; j < iNumPoints; j++)
    {
        for (int k = 0; k < m_iDimension; k++)
            akP[j][k] *= 1.0f+RANDOM_WIDTH*Mathf::SymmetricRandom(-1.0f);
    }
}     
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
void LCPPolyDist<VectorIn,TVector,TTuple>::RandomizeHalfspaces ()
{
    // small values (0.05) of RANDOM_WIDTH tolerated with sample data files
    int j, k;
    for (j = 0; j < m_iNumFaces1; j++)
    {
        m_adB1[j] *= 1.0f+RANDOM_WIDTH*Mathf::SymmetricRandom(-1.0f);
        for (k = 0; k < m_iDimension; k++)
            m_akA1[j][k] *= 1.0f+RANDOM_WIDTH*Mathf::SymmetricRandom(-1.0f);
    }
    for (j = 0; j < m_iNumFaces2; j++)
    {
        m_adB2[j] *= 1.0f+RANDOM_WIDTH*Mathf::SymmetricRandom(-1.0f);
        for (k = 0; k < m_iDimension; k++)
            m_akA2[j][k] *= 1.0f+RANDOM_WIDTH*Mathf::SymmetricRandom(-1.0f);
    }
}
//----------------------------------------------------------------------------
template <class VectorIn, class TVector, class TTuple>
void LCPPolyDist<VectorIn,TVector,TTuple>::LogVerifyFailure (
    int iWhichPolyhedron, int i, double fDiff)
{
    m_kLog << "Solution fails to meet constraint " << i;
    if ( iWhichPolyhedron == 1 )
        m_kLog << " for the first polyhedron." << endl;
    else
        m_kLog << " for the second polyhedron." << endl;           
    m_kLog << "The error is " << fDiff << endl;
}
//----------------------------------------------------------------------------
#endif

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM LCPPolyDist<Vector2f,Vector2d,Tuple2>;
double LCPPolyDist2::VERIFY_MIN_DIFFERENCE = 1e-10;
double LCPPolyDist2::RANDOM_WIDTH = 0.0;
template class WML_ITEM LCPPolyDist<Vector3f,Vector3d,Tuple3>;
double LCPPolyDist3::VERIFY_MIN_DIFFERENCE = 1e-10;
double LCPPolyDist3::RANDOM_WIDTH = 0.0;
}
//----------------------------------------------------------------------------
