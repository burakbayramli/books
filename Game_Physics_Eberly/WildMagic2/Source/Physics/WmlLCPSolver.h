// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef WMLLCPSOLVER_H
#define WMLLCPSOLVER_H

#include "WmlVector3.h"
#include <fstream>

namespace Wml
{

class WML_ITEM LCPSolver
{
public:
    // A class for solving the Linear Complementarity Problem (LCP)
    // w = Mz + q, w o z = 0, w >= 0, z >= 0.
    //
    // Input:
    //   N is the number of equations: iNumEquations
    //   M is a positive semidefinite matrix of size N: aadM.
    //   q is a vector of reals: adQ.
    //
    // Output:
    //   z and w are the solutions: adZ, adW.
    //   status of results: riStatus

    enum // status codes
    {
        SC_FOUND_SOLUTION,               // solution
        SC_FOUND_TRIVIAL_SOLUTION,       // solution (z = 0, w = q)
        SC_CANNOT_REMOVE_COMPLEMENTARY,  // no solution (unbounded)
        SC_EXCEEDED_MAX_RETRIES,         // no solution (round-off problems?)
    };

    LCPSolver (int iNumEquations, double** aadM, double* adQ, double* adZ,
        double* adW, int& riStatus);

    // In theory, one iteration of the LCP solver should work.  Floating
    // point round-off errors can cause the solver to fail.  When this does,
    // the solver is allowed to retry the algorithm with a different order
    // of input equations.  The maximum number of retries of this type is
    // specified by this static variable.  The default is 100.
    static int MAX_RETRIES;

    // The search for a pivot equation uses a comparison of a certain term
    // to zero.  To deal with floating point round-off errors, the comparison
    // is based on a small tolerance about zero.  The default is 0.0.
    static double ZERO_TOLERANCE;

    // The solver computes constant coefficients, z coefficients, and w
    // coefficients.  If any of these are nearly zero, their values are set
    // to zero.  The decision is made based on a relative comparison of a
    // ratio to zero.  The default is 0.0.
    static double RATIO_ERROR;

private:
    void AllocateEquations ();
    void DeallocateEquations ();
    bool InitializeEquations ();
    bool SelectEquation (int& riEqu);
    bool FindEquation (int& riEqu);
    bool EquationAlgorithm (int& riEqu);
    void Solve (char cBasicVariable, int iBasicVariableIndex);

    struct Equation
    {
        char Var;  // 'w' or 'z' are the only choices
        int VarIndex;  // index of the w or z variable
        double* C;  // constant coefficients
        double* W;  // coefficients of the w terms
        double* Z;  // coefficients of the z terms
    };

    int m_iNumEquations;
    double** m_aadM;
    double* m_adQ;
    Equation* m_akEq;
    char m_cNonBasicVariable;
    char m_cDepartingVariable;
    int m_iNonBasicVariableIndex;
    int m_iDepartingVariableIndex;
    double m_dFuzz;

// for writing messages to a log file during testing and debugging
//#define LCPSOLVER_LOG
#ifdef LCPSOLVER_LOG
    #define LOGFUNCTION(func) func;
    void OpenLog ();
    void CloseLog ();
    void PrintImmediateSolution ();
    void PrintCannotRemoveComplementary ();
    void PrintNoBasicVariable (char cBasicVariable, int iBasicVariableIndex);
    void PrintEquations ();
    void PrintResults (bool bZ0Basic);
    std::ofstream m_kLog;
    int m_iPrintEquationsCalls;
#else
    #define LOGFUNCTION(func)
#endif
};

}

#endif
