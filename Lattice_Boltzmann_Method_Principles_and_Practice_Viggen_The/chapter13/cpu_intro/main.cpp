/* This code accompanies
 *   The Lattice Boltzmann Method: Principles and Practice
 *   T. Krüger, H. Kusumaatmaja, A. Kuzmin, O. Shardt, G. Silva, E.M. Viggen
 *   ISBN 978-3-319-44649-3 (Electronic) 
 *        978-3-319-44647-9 (Print)
 *   http://www.springer.com/978-3-319-44647-9
 *
 * This code is provided under the MIT license. See LICENSE.txt.
 *
 * Author: Orest Shardt
 *
 */
#include <stdio.h>
#include <stdlib.h>
#define _USE_MATH_DEFINES
#include <math.h>

#include "seconds.h"

const unsigned int scale = 2;
const unsigned int NX = 32*scale;
const unsigned int NY = NX;

const unsigned int ndir = 9;
const size_t mem_size_ndir   = sizeof(double)*NX*NY*ndir;
const size_t mem_size_scalar = sizeof(double)*NX*NY;

const double w0 = 4.0/9.0;  // zero weight
const double ws = 1.0/9.0;  // adjacent weight
const double wd = 1.0/36.0; // diagonal weight
const double wi[] = {w0,ws,ws,ws,ws,wd,wd,wd,wd};
const int dirx[] = {0,1,0,-1, 0,1,-1,-1, 1};
const int diry[] = {0,0,1, 0,-1,1, 1,-1,-1};

const double nu = 1.0/6.0;
const double tau = 3.0*nu+0.5;

// Taylor-Green parameters
const double u_max = 0.04/scale;
const double rho0 = 1.0;

const unsigned int NSTEPS = 200*scale*scale;
const unsigned int NSAVE  =  50*scale*scale;
const unsigned int NMSG   =  50*scale*scale;

// compute L2 error and energy?
// disable for speed testing
const bool computeFlowProperties = true;

// suppress verbose output
const bool quiet = true;

// Declarations of functions used in main(). Definitions follow after main().
void taylor_green(unsigned int,unsigned int,unsigned int,double*,double*,double*);
void taylor_green(unsigned int,double*,double*,double*);
void stream(double*,double*);
void compute_rho_u(double*,double*,double*,double*);
void collide(double*,double*,double*,double*);
void init_equilibrium(double*,double*,double*,double*);
void compute_flow_properties(unsigned int,double*,double*,double*,double*);
void report_flow_properties(unsigned int,double*,double*,double*);
void save_scalar(const char*,double*,unsigned int);

inline size_t scalar_index(unsigned int x, unsigned int y)
{
    return NX*y+x;
}

inline size_t field_index(unsigned int x, unsigned int y, unsigned int d)
{
    return NX*(NY*d+y)+x;
}



int main(int argc, char* argv[])
{
    printf("Simulating Taylor-Green vortex decay\n");
    printf("      domain size: %ux%u\n",NX,NY);
    printf("               nu: %g\n",nu);
    printf("              tau: %g\n",tau);
    printf("            u_max: %g\n",u_max);
    printf("             rho0: %g\n",rho0);
    printf("        timesteps: %u\n",NSTEPS);
    printf("       save every: %u\n",NSAVE);
    printf("    message every: %u\n",NMSG);
    printf("\n");
    
    double bytesPerMiB = 1024.0*1024.0;
    
    double *f1  = (double*) malloc(mem_size_ndir);
    double *f2  = (double*) malloc(mem_size_ndir);
    double *rho = (double*) malloc(mem_size_scalar);
    double *ux  = (double*) malloc(mem_size_scalar);
    double *uy  = (double*) malloc(mem_size_scalar);
    
    size_t total_mem_bytes = 2*mem_size_ndir + 3*mem_size_scalar;
    
    if(f1 == NULL || f2 == NULL || rho == NULL || ux == NULL || uy == NULL)
    {
        fprintf(stderr,"Error: unable to allocate required memory (%.1f MiB).\n",total_mem_bytes/bytesPerMiB);
        exit(-1);
    }

    // compute Taylor-Green flow at t=0
    // to initialise rho, ux, uy fields.
    taylor_green(0,rho,ux,uy);
    
    // initialise f1 as equilibrium for rho, ux, uy
    init_equilibrium(f1,rho,ux,uy);
    
    save_scalar("rho",rho,0);
    save_scalar("ux", ux, 0);
    save_scalar("uy", uy, 0);
    if(computeFlowProperties)
    {
        report_flow_properties(0,rho,ux,uy);
    }
    
    double start = seconds();
    
    // main simulation loop; take NSTEPS time steps 
    for(unsigned int n = 0; n < NSTEPS; ++n)
    {
        // stream from f1 storing to f2 
        stream(f1,f2);
        
        // calculate post-streaming density and velocity 
        compute_rho_u(f2,rho,ux,uy);
        
        // perform collision on f2 
        collide(f2,rho,ux,uy);
        
        if((n+1)%NSAVE == 0)
        {
            save_scalar("rho",rho,n+1);
            save_scalar("ux", ux, n+1);
            save_scalar("uy", uy, n+1);
        }
        
        // swap pointers
        double *temp = f1;
        f1 = f2;
        f2 = temp;
        
        if((n+1)%NMSG == 0)
        {
            if(computeFlowProperties)
            {
                report_flow_properties(n+1,rho,ux,uy);
            }
            
            if(!quiet)
                printf("completed timestep %d\n",n+1);
        }
    }
    double end = seconds();
    double runtime = end-start;

    // note NX*NY overflows when NX=NY=65536
    size_t nodes_updated = NSTEPS*size_t(NX*NY);
    double speed = nodes_updated/(1e6*runtime);
    
    printf(" ----- performance information -----\n");
    printf(" memory allocated: %.1f (MiB)\n",total_mem_bytes/bytesPerMiB);
    printf("        timesteps: %u\n",NSTEPS);
    printf("          runtime: %.3f (s)\n",runtime);
    printf("            speed: %.2f (Mlups)\n",speed);
    
    // deallocate memory
    free(f1);  free(f2);
    free(rho); free(ux); free(uy);
    
    return 0;
}

void taylor_green(unsigned int t, unsigned int x, unsigned int y, double *r, double *u, double *v)
{
    double kx = 2.0*M_PI/NX;
    double ky = 2.0*M_PI/NY;
    double td = 1.0/(nu*(kx*kx+ky*ky));
    
    double X = x+0.5;
    double Y = y+0.5;
    double ux = -u_max*sqrt(ky/kx)*cos(kx*X)*sin(ky*Y)*exp(-1.0*t/td);
    double uy =  u_max*sqrt(kx/ky)*sin(kx*X)*cos(ky*Y)*exp(-1.0*t/td);
    double P = -0.25*rho0*u_max*u_max*((ky/kx)*cos(2.0*kx*X)+(kx/ky)*cos(2.0*ky*Y))*exp(-2.0*t/td);
    double rho = rho0+3.0*P;
    
    *r = rho;
    *u = ux;
    *v = uy;
}

void taylor_green(unsigned int t, double *r, double *u, double *v)
{
    for(unsigned int y = 0; y < NY; ++y)
    {
        for(unsigned int x = 0; x < NX; ++x)
        {
            size_t sidx = scalar_index(x,y);

            taylor_green(t,x,y,&r[sidx],&u[sidx],&v[sidx]);
        }
    }
}

void init_equilibrium(double *f, double *r, double *u, double *v)
{
    for(unsigned int y = 0; y < NY; ++y)
    {
        for(unsigned int x = 0; x < NX; ++x)
        {
            double rho = r[scalar_index(x,y)];
            double ux  = u[scalar_index(x,y)];
            double uy  = v[scalar_index(x,y)];
            
            for(unsigned int i = 0; i < ndir; ++i)
            {
                double cidotu = dirx[i]*ux + diry[i]*uy;
                f[field_index(x,y,i)] = wi[i]*rho*(1.0 + 3.0*cidotu + 4.5*cidotu*cidotu - 1.5*(ux*ux+uy*uy));
            }
        }
    }
}

void stream(double *f_src, double *f_dst)
{
    for(unsigned int y = 0; y < NY; ++y)
    {
        for(unsigned int x = 0; x < NX; ++x)
        {
            for(unsigned int i = 0; i < ndir; ++i)
            {
                // enforce periodicity
                // add NX to ensure that value is positive
                unsigned int xmd = (NX+x-dirx[i])%NX;
                unsigned int ymd = (NY+y-diry[i])%NY;
                
                f_dst[field_index(x,y,i)] = f_src[field_index(xmd,ymd,i)];
            }
        }
    }
}

void compute_rho_u(double *f, double *r, double *u, double *v)
{
    for(unsigned int y = 0; y < NY; ++y)
    {
        for(unsigned int x = 0; x < NX; ++x)
        {
            double rho = 0.0;
            double ux  = 0.0;
            double uy  = 0.0;
            
            for(unsigned int i = 0; i < ndir; ++i)
            {
                rho += f[field_index(x,y,i)];
                ux  += dirx[i]*f[field_index(x,y,i)];
                uy  += diry[i]*f[field_index(x,y,i)];
            }
            
            
            r[scalar_index(x,y)] = rho;
            u[scalar_index(x,y)] = ux/rho;
            v[scalar_index(x,y)] = uy/rho;
        }
    }
}

void collide(double *f, double *r, double *u, double *v)
{
    // useful constants
    const double tauinv = 2.0/(6.0*nu+1.0); // 1/tau
    const double omtauinv = 1.0-tauinv;     // 1 - 1/tau

    for(unsigned int y = 0; y < NY; ++y)
    {
        for(unsigned int x = 0; x < NX; ++x)
        {
            double rho = r[scalar_index(x,y)];
            double ux  = u[scalar_index(x,y)];
            double uy  = v[scalar_index(x,y)];
            
            for(unsigned int i = 0; i < ndir; ++i)
            {
                // calculate dot product
                double cidotu = dirx[i]*ux + diry[i]*uy;
                
                // calculate equilibrium
                double feq = wi[i]*rho*(1.0 + 3.0*cidotu + 4.5*cidotu*cidotu - 1.5*(ux*ux+uy*uy));
                
                // relax to equilibrium
                f[field_index(x,y,i)]  = omtauinv*f[field_index(x,y,i)] + tauinv*feq;
            }
        }
    }
}

void compute_flow_properties(unsigned int t, double *r, double *u, double *v, double *prop)
{
    // prop must point to space for 4 doubles:
    // 0: energy
    // 1: L2 error in rho
    // 2: L2 error in ux
    // 3: L2 error in uy
    
    double E = 0.0; // kinetic energy
    
    double sumrhoe2 = 0.0; // sum of error squared in rho
    double sumuxe2 = 0.0;  //                         ux
    double sumuye2 = 0.0;  //                         uy
    
    double sumrhoa2 = 0.0; // sum of analytical rho squared
    double sumuxa2 = 0.0;  //                   ux
    double sumuya2 = 0.0;  //                   uy
    
    for(unsigned int y = 0; y < NY; ++y)
    {
        for(unsigned int x = 0; x < NX; ++x)
        {
            double rho = r[scalar_index(x,y)];
            double ux  = u[scalar_index(x,y)];
            double uy  = v[scalar_index(x,y)];
            E += rho*(ux*ux + uy*uy);
            
            double rhoa, uxa, uya;
            taylor_green(t,x,y,&rhoa,&uxa,&uya);
            
            sumrhoe2 += (rho-rhoa)*(rho-rhoa);
            sumuxe2  += (ux-uxa)*(ux-uxa);
            sumuye2  += (uy-uya)*(uy-uya);

            sumrhoa2 += (rhoa-rho0)*(rhoa-rho0);
            sumuxa2  += uxa*uxa;
            sumuya2  += uya*uya;
        }
    }
    
    prop[0] = E;
    prop[1] = sqrt(sumrhoe2/sumrhoa2);
    prop[2] = sqrt(sumuxe2/sumuxa2);
    prop[3] = sqrt(sumuye2/sumuya2);
}

void report_flow_properties(unsigned int t, double *rho, double *ux, double *uy)
{
    double prop[4];
    compute_flow_properties(t,rho,ux,uy,prop);
    printf("%u,%g,%g,%g,%g\n",t,prop[0],prop[1],prop[2],prop[3]);
}

void save_scalar(const char* name, double *scalar, unsigned int n)
{
    // assume reasonably-sized file names
    char filename[128];
    char format[16];
    
    // compute maximum number of digits
    int ndigits = floor(log10((double)NSTEPS)+1.0);
    
    // generate format string
    // file name format is name0000nnn.bin
    sprintf(format,"%%s%%0%dd.bin",ndigits);
    sprintf(filename,format,name,n);
    
    // open file for writing
    FILE *fout = fopen(filename,"wb+");
    
    // write data
    fwrite(scalar,1,mem_size_scalar,fout);
    
    // close file
    fclose(fout);
    
    if(ferror(fout))
    {
        fprintf(stderr,"Error saving to %s\n",filename);
        perror("");
    }
    else
    {
        if(!quiet)
            printf("Saved to %s\n",filename);
    }
}


