// Robert Crandall
// Chan-Vese Segmentation

#include "ChanVeseSegmentation.h"
#include <iostream>
using std::cout;
using std::endl;
using std::atan;
using std::sqrt;
using std::abs;
using std::pow;
using std::min;
using std::max;

const double PI = 3.14159265358979323846264338327950288;

//---------------------------------------------------------------------------//
// function GetRegionAverages
// Compute c1 and c2 as used in the Chan-Vese segmentation algorithm.
// c1 and c2 are given by 
//         c1 = integral(u0*H(phi))dxdy/integral(H(phi))dxdy
//         c2 = integral(u0*(1-H(phi))dxdy/integral(1-H(phi))dxdy
//
// If epsilon == 0, we define H as the usual Heaviside function. Then  c1 is 
// the average of the image pixels over the set where phi is >= 0, and c2 is
// the average over {phi < 0}.  
// If epsilon > 0, we use a smoothed version of the Heaviside function with
// parameter epsilon.
void GetRegionAverages(Image<unsigned char>* img, 
                       Image<double>* phi,
                       double epsilon,
                       double &c1,
                       double &c2)
{
  // Non-smoothed calculation
  if (0 == epsilon)
  {
    int n1 = 0;
    int n2 = 0;
    double Iplus = 0;
    double Iminus = 0;

    for (unsigned int i = 0; i < img->nRows(); ++i)
    {
      for (unsigned int j = 0; j < img->nCols(); ++j)
      {
        if (phi->data()[i][j] >= 0)
        {
          ++n1;
          Iplus += (double)img->data()[i][j];
        }
        else
        {
          ++n2;
          Iminus += (double)img->data()[i][j];
        }
      }
    }
    c1 = Iplus/double(n1);
    c2 = Iminus/double(n2);
  }
  // Smoothed calculation
  else
  {
    double num1 = 0;
    double den1 = 0;
    double num2 = 0;
    double den2 = 0;
    double H_phi;
    for (unsigned int i = 0; i < phi->nRows(); ++i)
    {
      for (unsigned int j = 0; j < phi->nCols(); ++j)
      {
        // Compute value of H_eps(phi) where H_eps is a mollified Heavyside function
        H_phi = .5*(1+(2/PI)*atan(phi->data()[i][j]/epsilon));
        num1 += ((double)img->data()[i][j])*H_phi;
        den1 += H_phi;
        num2 += ((double)img->data()[i][j])*(1 - H_phi);
        den2 += 1 - H_phi;
      }
    }
    
    c1 = num1/den1;
    c2 = num2/den2;
  }
}

//---------------------------------------------------------------------------//
// function ReinitPhi
// Reinitialize phi to the signed distance function to its zero contour
void ReinitPhi(Image<double>* phiIn,
               Image<double>** psiOut,
               double dt,
               double h,
               unsigned int numIts)
{
  if (0 == *psiOut)
    (*psiOut) = new Image<double>(phiIn->nRows(),phiIn->nCols());
  else if ((*psiOut)->nRows() != phiIn->nRows()
        || (*psiOut)->nCols() != phiIn->nCols())
    (*psiOut)->Allocate(phiIn->nRows(),phiIn->nCols());
  
  (*psiOut)->CopyFrom(phiIn);
  
  double a;
  double b;
  double c;
  double d;
  double x;
  double G;
  
  bool fStop = false;
  double Q;
  unsigned int M;
  Image<double>* psiOld = new Image<double>(phiIn->nRows(),phiIn->nCols());
  
  for (unsigned int k = 0; k < numIts && fStop == false; ++k)
  {
    psiOld->CopyFrom(*psiOut);
    for (unsigned int i = 1; i < phiIn->nRows()-1; ++i)
    {
      for (unsigned int j = 1; j < phiIn->nCols()-1; ++j)
      {
        a = (phiIn->data()[i][j] - phiIn->data()[i-1][j])/h;
        b = (phiIn->data()[i+1][j] - phiIn->data()[i][j])/h;
        c = (phiIn->data()[i][j] - phiIn->data()[i][j-1])/h;
        d = (phiIn->data()[i][j+1] - phiIn->data()[i][j])/h;
        
        if (phiIn->data()[i][j] > 0)
          G = sqrt(max(max(a,0.0)*max(a,0.0),min(b,0.0)*min(b,0.0))
                 + max(max(c,0.0)*max(c,0.0),min(d,0.0)*min(d,0.0))) - 1.0;
        else if (phiIn->data()[i][j] < 0)
          G = sqrt(max(min(a,0.0)*min(a,0.0),max(b,0.0)*max(b,0.0))
                 + max(min(c,0.0)*min(c,0.0),max(d,0.0)*max(d,0.0))) - 1.0;
        else
          G = 0;
        
        x = (phiIn->data()[i][j] >= 0)?(1.0):(-1.0);
        (*psiOut)->data()[i][j] = (*psiOut)->data()[i][j] - dt*x*G;
      }
    }
    
    // Check stopping condition
    Q = 0.0;
    M = 0.0;
    for (unsigned int i = 0; i < phiIn->nRows(); ++i)
    {
      for (unsigned int j = 0; j < phiIn->nCols(); ++j)
      {
        if (abs(psiOld->data()[i][j]) <= h)
        {
          ++M;
          Q += abs(psiOld->data()[i][j] - (*psiOut)->data()[i][j]);
        }
      }
    }
    if (M != 0)
      Q = Q/((double)M);
    else
      Q = 0.0;
    
    if (Q < dt*h*h)
    {
      fStop = true;
      //cout << "Stopping condition reached at " << k+1 << " iterations; Q = " << Q << endl;
    }
    else
    {
      //cout << "Iteration " << k << ", Q = " << Q << " > " << dt*h*h << endl;
    }
  }
}

//---------------------------------------------------------------------------//
// function GetChanVeseCoefficients
// Compute coefficients needed in Chan-Vese segmentation algorithm given current 
// level set function
void GetChanVeseCoefficients(Image<double>* phi,
                             struct CVsetup* pCVinputs,
                             unsigned int i,
                             unsigned int j,
                             double L,
                             double& F1,
                             double& F2,
                             double& F3,
                             double& F4,
                             double& F,
                             double& deltaPhi)
{
  // factor to avoid division by zero
  double eps = 0.000001;
  double h = pCVinputs->h;
  double dt = pCVinputs->dt;
  double mu = pCVinputs->mu;
  unsigned int p = pCVinputs->p;
  
  double C1 = 1/sqrt(eps + pow((phi->data()[i+1][j] - phi->data()[i][j]),2)
                         + pow((phi->data()[i][j+1] - phi->data()[i][j-1]),2)/4.0);
  double C2 = 1/sqrt(eps + pow((phi->data()[i][j] - phi->data()[i-1][j]),2)
                         + pow((phi->data()[i-1][j+1] - phi->data()[i-1][j-1]),2)/4.0);
  double C3 = 1/sqrt(eps + pow((phi->data()[i+1][j] - phi->data()[i-1][j]),2)/4.0
                         + pow((phi->data()[i][j+1] - phi->data()[i][j]),2));
  double C4 = 1/sqrt(eps + pow((phi->data()[i+1][j-1] - phi->data()[i-1][j-1]),2)/4.0
                         + pow((phi->data()[i][j] - phi->data()[i][j-1]),2));

  deltaPhi = h/(PI*(h*h + (phi->data()[i][j])*(phi->data()[i][j])));
  
  double Factor = dt*deltaPhi*mu*(double(p)*pow(L,p-1));
  F = h/(h+Factor*(C1+C2+C3+C4));
  Factor = Factor/(h+Factor*(C1+C2+C3+C4));
  
  F1 = Factor*C1;
  F2 = Factor*C2;
  F3 = Factor*C3;
  F4 = Factor*C4;
}

//---------------------------------------------------------------------------//
// Main segmentation algorithm.  Segment a grayscale image into foreground and
// background regions, given an initial contour defined by the level set function
// phi.  Based on the algorithm described in the paper
// "Active Contours Without Edges" by Chan & Vese.
void ChanVeseSegmentation(Image<unsigned char>* img,
                          Image<double>* phi0,
                          Image<double>** phi,
                          struct CVsetup* pCVinputs)
{
  double P_ij;
  double deltaPhi;
  double F1;
  double F2; 
  double F3;
  double F4;
  double F;
  double L;
  double c1;
  double c2;
  
  // Segmentation parameters
  double h = pCVinputs->h;
  double dt = pCVinputs->dt;
  double nu = pCVinputs->nu;
  double lambda1 = pCVinputs->lambda1;
  double lambda2 = pCVinputs->lambda2;
  unsigned int p = pCVinputs->p;
  
  // Variables to evaluate stopping condition
  bool fStop = false;
  double Q;
  unsigned int M;
  Image<double>* phiOld = new Image<double>(img->nRows(),img->nCols());
  
  // Initialize phi
  if (0 == phi)
    *phi = new Image<double>(img->nRows(),img->nCols());
  else if ((*phi)->nRows() != phi0->nRows() || (*phi)->nCols() != phi0->nCols())
    (*phi)->Allocate(phi0->nRows(),phi0->nCols());
  
  (*phi)->CopyFrom(phi0);
  
  
  // Main loop
  for (unsigned int k = 0; k < 5 && fStop == false; ++k)
  {
    phiOld->CopyFrom(*phi);
    
    // Compute region averages for current level set function
    // Main segmentation algorithm
    GetRegionAverages(img, *phi, h, c1, c2);

    // Inner loop...
    for (unsigned int l = 0; l < 5; ++l)
    {
      // Compute length of contour if p > 1
      if (1 == p)
      {
        L = 1.0;
      }
      else
      {
        L = 1.0; // fix this!!
      }
      
      // Loop through all interior image pixels
      for (unsigned int i = 1; i < img->nRows()-1; ++i)
      {
        for (unsigned int j = 1; j < img->nCols()-1; ++j)
        {
          // Compute coefficients needed in update
          GetChanVeseCoefficients(*phi,
                                  pCVinputs,
                                  i, j,
                                  L,
                                  F1,
                                  F2,
                                  F3,
                                  F4,
                                  F,
                                  deltaPhi);

          P_ij = (*phi)->data()[i][j] 
                   - dt*deltaPhi*(nu + lambda1*pow(img->data()[i][j]-c1,2)
                                     - lambda2*pow(img->data()[i][j]-c2,2));

          // Update level set function
          (*phi)->data()[i][j] = F1*(*phi)->data()[i+1][j]
                               + F2*(*phi)->data()[i-1][j]
                               + F3*(*phi)->data()[i][j+1]
                               + F4*(*phi)->data()[i][j-1]
                               + F*P_ij;
        }
      }
      
      // Update border values of phi by reflection
      for (unsigned int i = 0; i < img->nRows(); ++i)
      {
        (*phi)->data()[i][0] = (*phi)->data()[i][1];
        (*phi)->data()[i][img->nCols()-1] = (*phi)->data()[i][img->nCols()-2];
      }
      for (unsigned int j = 0; j < img->nCols(); ++j)
      {
        (*phi)->data()[0][j] = (*phi)->data()[1][j];
        (*phi)->data()[img->nRows()-1][j] = (*phi)->data()[img->nRows()-2][j];
      }
 
      // Reinitialize phi to the signed distance function to its zero contour
      ReinitPhi(*phi, phi, 0.1, h, 100);
    }
    
    // Check stopping condition
    /*
    Q = 0.0;
    M = 0.0;
    for (unsigned int i = 0; i < img->nRows(); ++i)
    {
      for (unsigned int j = 0; j < img->nCols(); ++j)
      {
        if (abs(phiOld->data()[i][j]) <= h)
        {
          ++M;
          Q += abs(phiOld->data()[i][j] - (*phi)->data()[i][j]);
        }
      }
    }
    if (M != 0)
      Q = Q/((double)M);
    else
      Q = 0.0;
    
    if (Q < dt*h*h)
    {
      fStop = true;
      cout << "Stopping condition reached at " << k+1 << " iterations; Q = " << Q << endl;
    }
    else
    {
      cout << "Iteration " << k << ", Q = " << Q << " > " << dt*h*h << endl;
    }
    */
    
  }
}
