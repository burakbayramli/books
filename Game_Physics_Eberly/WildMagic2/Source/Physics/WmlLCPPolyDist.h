// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef WMLLCPPOLYDIST_H
#define WMLLCPPOLYDIST_H

#include "WmlTuple.h"
#include "WmlVector2.h"
#include "WmlVector3.h"
#include "WmlLCPSolver.h"
#include <fstream>
#include <string>

namespace Wml
{

template <class VectorIn, class TVector, class TTuple>
class WML_ITEM LCPPolyDist
{
public:
    // A class for computing the distance between two convex polygons or two
    // convex polyhedra.  The distance algorithm is formulated as a linear
    // complementarity problem (LCP) for three-dimensional data.  For convex
    // polygons in two dimensions, the third component of the vertices must
    // be zero.

    // Status codes returned by the polyhedra distance calculation.
    enum
    {
        SC_FOUND_SOLUTION
            = LCPSolver::SC_FOUND_SOLUTION, // solution

        SC_FOUND_TRIVIAL_SOLUTION
            = LCPSolver::SC_FOUND_TRIVIAL_SOLUTION, // solution (z = 0, w = q)

        SC_CANNOT_REMOVE_COMPLEMENTARY
            = LCPSolver::SC_CANNOT_REMOVE_COMPLEMENTARY, // no solution

        SC_EXCEEDED_MAX_RETRIES
            = LCPSolver::SC_EXCEEDED_MAX_RETRIES, 

        // no solution (round-off problems?)
        SC_VERIFY_FAILURE,  // VerifySolution failed
        SC_TEST_POINTS_TEST_FAILED, // VerifyWithTestPoints failed
    };

    // Polygons represented by vertices and edges.  The vertices are stored
    // as vectors in 3D with the last component always zero.
    //   iNumPoints1  = number of vertices of first polygon
    //   akPoint1     = array of vertices
    //   iNumFaces1   = number of edges (same as iNumPoints1)
    //   akFace1      = array of edges, each edge a pair of indices into the
    //                  vertex array akPoint1: <0,1>, <1,2>, ..., <n-2,n-1>,
    //                  <n-1,0>
    //   iNumPoints2  = number of vertices of second polygon
    //   akPoint2     = array of vertices
    //   iNumFaces2   = number of edges (same as iNumPoints2)
    //   akFace2      = array of edges, each edge a pair of indices into the
    //                  vertex array akPoint2: <0,1>, <1,2>, ..., <n-2,n-1>,
    //                  <n-1,0>
    //   riStatusCode = the status of the LCP numerical solver
    //   rfDistance   = distance between the polygons
    //   akRes        = array of two closest points, one on each polygon
    // 
    // Polyhedra represented by vertices and faces.
    //   iNumPoints1  = number of vertices of first polyhedron
    //   akPoint1     = array of vertices
    //   iNumFaces1   = number of triangular faces
    //   akFace1      = array of faces, each face a triple of indices into
    //                  the vertex array akPoint1
    //   iNumPoints2  = number of vertices of second polyhedron
    //   akPoint2     = array of vertices
    //   iNumFaces2   = number of triangular faces
    //   akFace2      = array of faces, each face a triple of indices into
    //                  the vertex array akPoint2
    //   riStatusCode = the status of the LCP numerical solver
    //   rfDistance   = distance between the polyhedra
    //   akClosest    = array of two closest points, one on each polyhedron

    LCPPolyDist (int iNumPoints1, VectorIn* akPoint1, int iNumFaces1,
        TTuple* akFace1, int iNumPoints2, VectorIn* akPoint2, int iNumFaces2,
        TTuple* akFace2, int& riStatusCode, float& rfDistance,
        VectorIn akClosest[/*2*/]);

    // Polygons and polyhedra represented by halfspaces.  The halfspaces are
    // all the points p such that p.Dot(akAn[i]) <= afBn[i], n = 1 or 2.  The
    // arrays akAn[] are the outer pointing edge/face normals.  The arrays
    // afBn[] are the line/plane constants.
    //   iNum1 = number of halfspaces of first polygon/polyhedron
    //   afB1  = array of line/plane constants
    //   akA1  = array of outer pointing edge/face normals
    //   iNum2 = number of halfspaces of second polygon/polyhedron
    //   afB2  = array of line/plane constants
    //   akA2  = array of outer pointing edge/face normals
    //   riStatusCode = the status of the LCP numerical solver
    //   rfDistance   = distance between the polygons/polyhedra
    //   akClosest    = array of two closest points, one on each
    //                  polygon/polyhedron

    LCPPolyDist (int iNum1, float* afB1, VectorIn* akA1, int iNum2,
        float* afB2, VectorIn* akA2, int& riStatusCode, float& rfDistance,
        VectorIn akClosest[/*2*/]);

    // Solution vectors are tested to determine if they meet constraints
    // imposed by the halfspace statement of the problem: V o Z <= B. 
    // VERIFY_MIN_DIFFERENCE is the amount that this dot product can exceed
    // B without reporting an error. Default value is 0.00001.
    static double VERIFY_MIN_DIFFERENCE;

    // Used in GenerateHalfSpaceDescription to produce a random array of 
    // vertices whose means are the input values to FuzzArray. Each vertex 
    // is selected from a uniform distribution of width RANDOM_WIDTH. 
    // Default value = 0.0, i.e. no random variation.
    static double RANDOM_WIDTH;

private:
    double ProcessLoop (bool bHS, int& riError, VectorIn* akRes);
    bool BuildMatrices (double** aadM, double* adQ);
    void ComputeHalfspaces (int iNumPoints, const TVector* akP,
        int iNumFaces, const TTuple* akF, TVector* akA, double* afB);
    void GenerateHalfSpaceDescription (int iNumPoints, TVector* akP1,
        int iNumFaces, TTuple* akF, double* afB, TVector* akA);
    void ChangeFaceOrder (int iNumFaces, TTuple* akF);
    void MoveHalfspaces (int iNumFaces, double* afB, TVector* akA);
    void MovePoints ();
    int VerifySolution (const TVector* akRes);

    int m_iDimension;
    int m_iNumEquations;
    int m_iNumPoints1;
    int m_iNumPoints2;
    int m_iNumFaces1;
    int m_iNumFaces2; 
    double* m_adB1;  // halfspace constants
    double* m_adB2;
    TVector* m_akA1;  // halfspace vectors
    TVector* m_akA2;
    TVector* m_akP1;  // points
    TVector* m_akP2;
    TTuple* m_akF1;  // faces
    TTuple* m_akF2;

// For writing messages to a log file during testing and debugging.  If you
// enable this, the distance calculator significantly slows down.
//
//#define LCPPOLYDIST_LOG
#ifdef LCPPOLYDIST_LOG
    #define LOGFUNCTION(func) func;
#else
    #define LOGFUNCTION(func)
#endif

#ifdef LCPPOLYDIST_LOG
    void OpenLog ();
    void CloseLog ();
    void PrintMatrices (double** aadM, double* adQ);
    void LogRetries (int iTryNumber);
    void LCPSolverLoopLimit ();
    void LogVertices (const TVector* akP, int iNum);
    void LogVerticesAndFaces (int iNumPoints, const VectorIn* akP,
        int iNumFaces, const TTuple* akF);
    void LogHalfspaces (const double* afB, const TVector* akP, int iNum);
    void SetupTestPoints (int iI, int iNumFaces, int iNumPoints, 
        TVector kZRes, const double* afB, const TVector* akA,
        const TVector* akP, TVector* akTestPoints);
    void LogTestPoints (const TVector* akTestPoints);
    void LogSolutionWithTestPoints (int iI, TVector kZResult, 
        const TVector* akTestPoints);
    void VerifyWithTestPoints (const TVector* akRes, int& riError);
    void RandomizeArray (int iNumPoints, TVector* akP);
    void RandomizeHalfspaces ();
    void LogVerifyFailure (int iWhich, int i, double fDiff);
    std::ofstream m_kLog;
#endif
};

typedef Tuple<2> Tuple2;
typedef Tuple<3> Tuple3;
typedef LCPPolyDist<Vector2f,Vector2d,Tuple2> LCPPolyDist2;
typedef LCPPolyDist<Vector3f,Vector3d,Tuple3> LCPPolyDist3;

}

#endif
