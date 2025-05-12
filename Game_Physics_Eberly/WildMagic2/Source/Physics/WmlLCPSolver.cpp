// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "WmlLCPSolver.h"
using namespace Wml;

int LCPSolver::MAX_RETRIES = 100;
double LCPSolver::ZERO_TOLERANCE = 0.0;
double LCPSolver::RATIO_ERROR = 0.0;

//----------------------------------------------------------------------------
LCPSolver::LCPSolver (int iNumEquations, double** aadM, double* adQ,
    double* adZ, double* adW, int& riStatus)
{
    m_iNumEquations = iNumEquations;
    m_adQ = adQ;
    m_aadM = aadM;

    LOGFUNCTION(OpenLog())

    AllocateEquations();

    if ( InitializeEquations() )
    {
        LOGFUNCTION(PrintEquations())

        int iTry;
        for (iTry = 0; iTry < MAX_RETRIES; iTry++)
        {
            int iEqu;
            if ( !SelectEquation(iEqu) )
            {
                LOGFUNCTION(PrintCannotRemoveComplementary())
                riStatus = SC_CANNOT_REMOVE_COMPLEMENTARY;
                break;
            }

            Solve(m_akEq[iEqu-1].Var,m_akEq[iEqu-1].VarIndex);

            LOGFUNCTION(PrintEquations())

            // determine if z0 is a basic variable
            bool bZ0Basic = false;
            int i;
            for (i = 0; i < m_iNumEquations; i++)
            {
                if ( m_akEq[i].Var == 'z' && m_akEq[i].VarIndex == 0 )
                {
                    bZ0Basic = true;
                    break;
                }
            }

            if ( !bZ0Basic )
            {
                // solution found when z0 is removed from the basic set
                memset(adZ,0,m_iNumEquations*sizeof(double));
                memset(adW,0,m_iNumEquations*sizeof(double));
                for (i = 0; i < m_iNumEquations; i++)
                {
                    if ( m_akEq[i].Var == 'z' )
                        adZ[m_akEq[i].VarIndex-1] = m_akEq[i].C[0];
                    else
                        adW[m_akEq[i].VarIndex-1] = m_akEq[i].C[0];
                }
                riStatus = SC_FOUND_SOLUTION;

                LOGFUNCTION(PrintResults(bZ0Basic))

                break;
            }
        }

        if ( iTry == MAX_RETRIES )
            riStatus = SC_EXCEEDED_MAX_RETRIES;
    }
    else
    {
        LOGFUNCTION(PrintImmediateSolution())
        memset(adZ,0,m_iNumEquations*sizeof(double));
        memcpy(adW,adQ,m_iNumEquations*sizeof(double));
        riStatus = SC_FOUND_TRIVIAL_SOLUTION;
    }

    DeallocateEquations();
    LOGFUNCTION(CloseLog())
}
//----------------------------------------------------------------------------
void LCPSolver::AllocateEquations ()
{
    m_akEq = new Equation[m_iNumEquations];
    for (int i = 0; i < m_iNumEquations; i++)
    {
        m_akEq[i].C = new double[m_iNumEquations+1];
        m_akEq[i].W = new double[m_iNumEquations+1];
        m_akEq[i].Z = new double[m_iNumEquations+1];
    }
}
//----------------------------------------------------------------------------
void LCPSolver::DeallocateEquations ()
{
    for (int i = 0; i < m_iNumEquations; i++)
    {
        delete[] m_akEq[i].C;
        delete[] m_akEq[i].W;
        delete[] m_akEq[i].Z;
    }
    delete[] m_akEq;
}
//----------------------------------------------------------------------------
bool LCPSolver::InitializeEquations ()
{
    int iNumEqP1 = m_iNumEquations + 1;
    int i;
    for (i = 0; i < m_iNumEquations; i++)
    {
        // initially w's are basic, z's are non-basic
        m_akEq[i].Var = 'w';

        // w indices run from 1 to iNumEquations
        m_akEq[i].VarIndex = i+1;

        // the "extra" variable in the equations is z0.
        memset(m_akEq[i].C,0,iNumEqP1*sizeof(double));
        memset(m_akEq[i].W,0,iNumEqP1*sizeof(double));
        memset(m_akEq[i].Z,0,iNumEqP1*sizeof(double));
        m_akEq[i].Z[0] = 1.0;
        m_akEq[i].C[i+1] = 1.0;
    }

    // Check if all the constant terms are nonnegative.  If so, the solution
    // is z = 0 and w = constant_terms.  The caller will set the values of z
    // and w, so just return from here.
    double dConstTermMin = 0.0;
    for (i = 0; i < m_iNumEquations; i++)
    {
        m_akEq[i].C[0] = m_adQ[i];
        if ( m_adQ[i] < dConstTermMin )
            dConstTermMin = m_adQ[i];
    }
    if ( dConstTermMin >= 0.0 )
        return false;

    // enter Z terms
    int j;
    for (i = 0; i < m_iNumEquations; i++)
    {
        // set m_adEq.Z[0] to 0.0 for any row in which all m_aadM are 0.0.
        double dRowOfZeros = 0.0;
        for (j = 0; j < m_iNumEquations; j++)
        {
            double dTemp = m_aadM[i][j];
            m_akEq[i].Z[j+1] = dTemp;
            if ( dTemp != 0.0 )
                dRowOfZeros = 1.0;
        }
        m_akEq[i].Z[0] *= dRowOfZeros;
    }

    for (i = 0; i < m_iNumEquations; i++)
    {
        // Find the max abs value of the coefficients on each row and divide
        // each row by that max abs value.
        double dMaxAbs = 0.0;
        for (j = 0; j < iNumEqP1; j++)
        {
            double dAbs = Mathd::FAbs(m_akEq[i].C[j]);
            if ( dAbs > dMaxAbs )
                dMaxAbs = dAbs;

            dAbs = Mathd::FAbs(m_akEq[i].W[j]);
            if ( dAbs > dMaxAbs )
                dMaxAbs = dAbs;

            dAbs = Mathd::FAbs(m_akEq[i].Z[j]);
            if ( dAbs > dMaxAbs )
                dMaxAbs = dAbs;
        }

        double dInvMaxAbs = 1.0/dMaxAbs;
        for (j = 0; j < iNumEqP1; j++)
        {
            m_akEq[i].C[j] *= dInvMaxAbs;
            m_akEq[i].W[j] *= dInvMaxAbs;
            m_akEq[i].Z[j] *= dInvMaxAbs;
        }       
    }
    return true;
}
//----------------------------------------------------------------------------
bool LCPSolver::SelectEquation (int& riEqu)
{
    // The algorithm for selecting the equation to be solved is:
    // 1. if z0 is not a basic variable, solve for z0
    //      choose the equation with smallest (negative) constant term.
    // 2. if a w, say wj, has just left the basic set, solve for zj.
    //      choose the equation to solve for zj by:
    //              coefficient, cj, of zj is negative
    //              the ratio constj/-cj is smallest.

    // determine if z0 is a basic variable
    bool bZ0Basic = false;
    for (int i = 0; i < m_iNumEquations; i++)
    {
        if ( m_akEq[i].Var == 'z' && m_akEq[i].VarIndex == 0 )
            bZ0Basic = true;
    }

    // If z0 is not basic, find the equation with the smallest (negative)
    // constant term and solve that equation for z0.
    if ( !bZ0Basic )
    {
        m_iDepartingVariableIndex = 0;
        m_cNonBasicVariable = 'z';
        m_iNonBasicVariableIndex = 0;
    }
    else  // z0 is basic
    {
        // Since the departing variable left the dictionary, solve for the
        // complementary variable.
        m_cNonBasicVariable = ( m_cDepartingVariable == 'w' ? 'z' : 'w' );
    }

    bool bFound = FindEquation(riEqu);
    if ( bFound )
    {
        m_iNonBasicVariableIndex = m_iDepartingVariableIndex;
        m_cDepartingVariable = m_akEq[riEqu-1].Var;
        m_iDepartingVariableIndex = m_akEq[riEqu-1].VarIndex;

    }
    return bFound;
}
//----------------------------------------------------------------------------
bool LCPSolver::FindEquation (int& riEqu)
{
    if ( m_iDepartingVariableIndex != 0 )
    {
        // Find the limiting equation for variables other than z0.  The
        // coefficient of the variable must be negative.  The ratio of the
        // constant polynomial to the negative of the smallest coefficient
        // of the variable is sought.   The constant polynomial must be
        // evaluated to compute this ratio.  It must be evaluated at a value
        // of the variable, dEpsi, such that the ratio remains smallest for
        // all smaller dEpsi.
        return EquationAlgorithm(riEqu);
    }

    // Special case for nonbasic z0; the coefficients are 1.  Find the
    // limiting equation when solving for z0.  At least one C[0] must be
    // negative initially or we start with a solution.  If all of the
    // negative constant terms are different, pick the equation with the
    // smallest (negative) ratio of constant term to the coefficient of
    // z0.  If several equations contain the smallest negative constant
    // term, pick the one with the highest coefficient for that one
    // contains dEpsi to the largest exponent.  NOTE: This is equivalent
    // to using the constant term polynomial in dEpsi but avoids
    // evaluating it.
    double dMin = 0.0;
    for (int i = 0; i < m_iNumEquations; i++)
    {
        if ( m_akEq[i].Z[0] != 0.0 )
        {
            double dQuot = m_akEq[i].C[0]/m_akEq[i].Z[0];
            if ( dQuot <= dMin || dMin == 0.0 )
            {
                dMin = dQuot;
                riEqu = i+1;
            }
        }
    }
    return ( dMin < 0.0 );
}
//----------------------------------------------------------------------------
bool LCPSolver::EquationAlgorithm (int& riEqu)
{
    // This code loops through the rows of the z or w array to find all the
    // terms for which the coefficient of the chosen term is negative.  The
    // row search is reduced to these.  For the columns of the constants array
    // the rows (equations) for which the ratios of the constant terms to the
    // z or w coefficients of interest is smallest are found. If there are
    // several such rows, they are noted.  The row search is further reduced
    // to these.  Proceed to the next column until there is only one row left.

    int (*aaiFoundArray)[2] = new int[m_iNumEquations+1][2];

    // Find equations with negative coefficients for selected index.
    double dTemp;
    int i, j;
    for (i = 0, j = 0; i < m_iNumEquations; i++)
    {                                    
        if ( m_cNonBasicVariable == 'z' )
            dTemp = m_akEq[i].Z[m_iDepartingVariableIndex];
        else
            dTemp = m_akEq[i].W[m_iDepartingVariableIndex];

        if ( dTemp < 0.0 )
            aaiFoundArray[j++][0] = i;
    }

    if ( j != 0 )  // no terms with negative coefficients
    {
        aaiFoundArray[j][0] = -1;

        // Find equation with smallest ratio of constTerm (polynomial) to 
        // selected (NonBasicVariable, DepartingVariableIndex) coefficient.
        int iFAI1 = 0, iFAI2 = 1;
        for (i = 0; i < m_iNumEquations+1; i++)
        {
            iFAI2 = ( iFAI1 == 0 ? 1 : 0 );

            int iFI1 = 0, iFI2 = 0;
            int j1 = aaiFoundArray[iFI1++][iFAI1];
            aaiFoundArray[iFI2++][iFAI2] = j1;
            int k = iFI1;
            while ( aaiFoundArray[k][iFAI1] > -1 )
            {
                int j2 = aaiFoundArray[k][iFAI1];
                if ( j2 < 0 )
                    break;

                double dDenom1, dDenom2;
                if ( m_cNonBasicVariable == 'z' )
                {
                    dDenom1 = m_akEq[j1].Z[m_iDepartingVariableIndex];
                    dDenom2 = m_akEq[j2].Z[m_iDepartingVariableIndex];
                }
                else
                {
                    dDenom1 = m_akEq[j1].W[m_iDepartingVariableIndex]; 
                    dDenom2 = m_akEq[j2].W[m_iDepartingVariableIndex]; 
                }
                dTemp = m_akEq[j2].C[i]/dDenom2 - m_akEq[j1].C[i]/dDenom1;
                if ( dTemp < -ZERO_TOLERANCE )       
                {
                    // The first equation has the smallest ratio.  Do nothing;
                    // the first equation is the choice.
                }
                else if ( dTemp > ZERO_TOLERANCE ) 
                {
                    // The second equation has the smallest ratio.
                    iFI1 = k;  // make second equation comparison standard
                    iFI2 = 0;  // restart the found array index
                    j1 = aaiFoundArray[iFI1++][iFAI1];
                    aaiFoundArray[iFI2++][iFAI2] = j1;
                }
                else  // the ratios are the same
                {
                    aaiFoundArray[iFI2++][iFAI2] = j2;
                }
                k++;
                aaiFoundArray[iFI2][iFAI2] = -1;
            }

            if ( iFI2 == 1 )
            {
                // "correct" exit
                riEqu = aaiFoundArray[0][iFAI2]+1;
                delete[] aaiFoundArray;
                return true;
            }

            iFAI1 = ( iFAI1 == 0 ? 1 : 0 );
        }
    }

    // NOTE:  We should never get here.
    // assert(false);
    delete[] aaiFoundArray;
    return false;
}
//----------------------------------------------------------------------------
void LCPSolver::Solve (char cBasicVariable, int iBasicVariableIndex)
{
    int iFound = -1, i ,j;
    for (i = 0; i < m_iNumEquations; i++)
    {
        if ( m_akEq[i].Var == cBasicVariable )
        {
            if ( m_akEq[i].VarIndex == iBasicVariableIndex )
                iFound = i;
        }
    }
    if ( iFound < 0 || iFound > m_iNumEquations-1 )
    {
        LOGFUNCTION(PrintNoBasicVariable(cBasicVariable,iBasicVariableIndex))
        return;
    }

    // the equation for the replacement variable in this cycle
    Equation kRep;
    kRep.Var = m_cNonBasicVariable;
    kRep.VarIndex = m_iNonBasicVariableIndex;
    kRep.C = new double[m_iNumEquations+1];
    kRep.W = new double[m_iNumEquations+1];
    kRep.Z = new double[m_iNumEquations+1];

    double dDenom;
    if ( m_cNonBasicVariable == 'z' )
        dDenom = -m_akEq[iFound].Z[m_iNonBasicVariableIndex];
    else
        dDenom = -m_akEq[iFound].W[m_iNonBasicVariableIndex];

    double dInvDenom = 1.0/dDenom;
    for (i = 0; i < m_iNumEquations+1; i++)
    {
        kRep.C[i] = m_akEq[iFound].C[i]*dInvDenom;
        kRep.W[i] = m_akEq[iFound].W[i]*dInvDenom;
        kRep.Z[i] = m_akEq[iFound].Z[i]*dInvDenom;
    }

    if ( m_cNonBasicVariable == 'z' )
        kRep.Z[m_iNonBasicVariableIndex] = 0.0;
    else
        kRep.W[m_iNonBasicVariableIndex] = 0.0;

    if ( cBasicVariable == 'z' )
        kRep.Z[iBasicVariableIndex] = -dInvDenom;
    else
        kRep.W[iBasicVariableIndex] = -dInvDenom;

    int iNumEqP1 = m_iNumEquations + 1;
    for (i = 0; i < m_iNumEquations; i++)
    {
        if ( i != iFound )      
        {
            double dCoef;
            if ( kRep.Var == 'z' )
                dCoef = m_akEq[i].Z[m_iNonBasicVariableIndex];
            else
                dCoef = m_akEq[i].W[m_iNonBasicVariableIndex];

            if ( dCoef != 0.0 )
            {
                for (j = 0; j < iNumEqP1; j++)
                {
                    m_akEq[i].C[j] += dCoef*kRep.C[j];
                    if ( Mathd::FAbs(m_akEq[i].C[j]) <
                         RATIO_ERROR*Mathd::FAbs(kRep.C[j]))
                    {
                        m_akEq[i].C[j] = 0.0;
                    }

                    m_akEq[i].W[j] += dCoef*kRep.W[j];
                    if ( Mathd::FAbs(m_akEq[i].W[j]) <
                         RATIO_ERROR*Mathd::FAbs(kRep.W[j]))
                    {
                        m_akEq[i].W[j] = 0.0;
                    }

                    m_akEq[i].Z[j] += dCoef*kRep.Z[j];
                    if ( Mathd::FAbs(m_akEq[i].Z[j]) <
                         RATIO_ERROR*Mathd::FAbs(kRep.Z[j]))
                    {
                        m_akEq[i].Z[j] = 0.0;
                    }
                }

                if ( kRep.Var == 'z' )
                    m_akEq[i].Z[kRep.VarIndex] = 0.0;
                else
                    m_akEq[i].W[kRep.VarIndex] = 0.0;
            }
        }
    }

    // replace the row corresponding to this equation
    m_akEq[iFound].Var = kRep.Var;
    m_akEq[iFound].VarIndex = kRep.VarIndex;
    memcpy(m_akEq[iFound].C,kRep.C,iNumEqP1*sizeof(double));
    memcpy(m_akEq[iFound].W,kRep.W,iNumEqP1*sizeof(double));
    memcpy(m_akEq[iFound].Z,kRep.Z,iNumEqP1*sizeof(double));

    delete[] kRep.C;
    delete[] kRep.W;
    delete[] kRep.Z;
}
//----------------------------------------------------------------------------

#ifdef LCPSOLVER_LOG
using namespace std;
//----------------------------------------------------------------------------
void LCPSolver::OpenLog ()
{
    // open the log file
    m_kLog.open("LCPSolver.log");
    assert( m_kLog );
    if ( !m_kLog )
        return;

    // print a header
    m_kLog << "LCPSolver" << endl;

    // print the current date and time
    time_t kClock;
    time(&kClock);
    m_kLog << "Time: " << asctime(localtime(&kClock)) << endl << endl;

    // use scientific notation for floating point output
    m_kLog.setf(ios::scientific,ios::floatfield);

    // print the input to the solver
    m_kLog << "There are " << m_iNumEquations
        << " rows and columns in this problem." << endl << endl;

    m_kLog << "The matrix M." << endl;
    int i;
    for (i = 0; i < m_iNumEquations; i++)
    {
        for (int j = 0; j < m_iNumEquations; j++)
            m_kLog << showpos << m_aadM[i][j] << ' ';
        m_kLog << endl;
    }

    m_kLog << "The vector Q." << endl;
    for (i = 0; i < m_iNumEquations; i++)
        m_kLog << showpos << m_adQ[i] << ' ';
    m_kLog << endl;

    // counter for number of times LogPrintEquations is called
    m_iPrintEquationsCalls = 0;
}
//----------------------------------------------------------------------------
void LCPSolver::CloseLog ()
{
    m_kLog.close();
}
//----------------------------------------------------------------------------
void LCPSolver::PrintImmediateSolution ()
{
    m_kLog << "Since the constants are all >= 0, the solution is " << endl;
    m_kLog << "adZ = 0, adW = adQ." << endl;
}
//----------------------------------------------------------------------------
void LCPSolver::PrintCannotRemoveComplementary ()
{
    m_kLog << "LCPSolver cannot remove complementary variable." << endl;
}
//----------------------------------------------------------------------------
void LCPSolver::PrintNoBasicVariable (char cBasicVariable,
    int iBasicVariableIndex)
{
    m_kLog << "No equation found for cBasicVariable " << cBasicVariable;
    m_kLog << " with iBasicVariableIndex " << iBasicVariableIndex << endl;
}
//----------------------------------------------------------------------------
void LCPSolver::PrintEquations ()
{
    m_kLog << endl << endl << "This is call " << ++m_iPrintEquationsCalls
           << " to LogPrintEquations." << endl;

    for (int i = 0; i < m_iNumEquations; i++)
    {
        m_kLog << endl << m_akEq[i].Var << "(" << m_akEq[i].VarIndex
            << ") = " << m_akEq[i].C[0];

        int j;
        for (j = 1; j < m_iNumEquations+1; j++)
        {
            if ( m_akEq[i].W[j] != 0.0 )
                m_kLog << showpos << m_akEq[i].W[j] << "*w(" << j << ")";
        }
        for (j = 0; j < m_iNumEquations+1; j++)
        {
            if ( m_akEq[i].Z[j] != 0.0 )
                m_kLog << showpos << m_akEq[i].Z[j] << "*z(" << j << ")";
        }
        m_kLog << endl;

        for (j = 1; j <= m_iNumEquations; j++)
        {
            if ( m_akEq[i].C[j] != 0.0 )
                m_kLog << showpos << m_akEq[i].C[j] << "*e^(" << j << "}";
        }
    }
}
//----------------------------------------------------------------------------
void LCPSolver::PrintResults (bool /* bZ0Basic */)
{
    // TO DO.  The parameter bZ0Basic is not used.  Should it be removed?

    int i, k;

    for (i = 0; i < m_iNumEquations; i++)
    {
        m_kLog << endl;
        m_kLog << m_akEq[i].Var << '(' << m_akEq[i].VarIndex << ") = ";
        m_kLog << m_akEq[i].C[0] << ' ';

        // k counts the position on a row
        for (k = 0; k < m_iNumEquations+1; k++)     // w terms
        {   
            if ( m_akEq[i].W[k] != 0 )        // skip zero terms
            {
                m_kLog << m_akEq[i].W[k];
                m_kLog << "*w (" << k << ')';
            }
        }
        for (k = 0; k < m_iNumEquations+1; k++)     // z terms
        {   
            if ( m_akEq[i].Z[k] != 0 )        // skip zero terms
            {
                m_kLog << m_akEq[i].Z[k];
                m_kLog << "*z (" << k << ')';
            }
        }
    }
    m_kLog << endl;
}
//----------------------------------------------------------------------------
#endif
