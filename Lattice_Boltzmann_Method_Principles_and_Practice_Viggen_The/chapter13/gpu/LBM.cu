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
#define _USE_MATH_DEFINES
#include <math.h>

#include <cuda.h>

#include "LBM.h"

const int nThreads = 32;

__device__ __forceinline__ size_t gpu_field0_index(unsigned int x, unsigned int y)
{
    return NX*y+x;
}

__device__ __forceinline__ size_t gpu_scalar_index(unsigned int x, unsigned int y)
{
    return NX*y+x;
}

__device__ __forceinline__ size_t gpu_fieldn_index(unsigned int x, unsigned int y, unsigned int d)
{
    return (NX*(NY*(d-1)+y)+x);
}

#define checkCudaErrors(err)  __checkCudaErrors(err,#err,__FILE__,__LINE__)
#define getLastCudaError(msg)  __getLastCudaError(msg,__FILE__,__LINE__)

inline void __checkCudaErrors(cudaError_t err, const char *const func, const char *const file, const int line )
{
    if(err != cudaSuccess)
    {
        fprintf(stderr, "CUDA error at %s(%d)\"%s\": [%d] %s.\n",
                file, line, func, (int)err, cudaGetErrorString(err));
        exit(-1);
    }
}

inline void __getLastCudaError(const char *const errorMessage, const char *const file, const int line )
{
    cudaError_t err = cudaGetLastError();
    if(err != cudaSuccess) {
        fprintf(stderr, "CUDA error at %s(%d): [%d] %s.\n",
                file, line, (int)err, cudaGetErrorString(err));
        exit(-1);
    }
}

// forward declarations of kernels
__global__ void gpu_taylor_green(unsigned int,double*,double*,double*);
__global__ void gpu_init_equilibrium(double*,double*,double*,double*,double*);
__global__ void gpu_stream_collide_save(double*,double*,double*,double*,double*,double*,bool);
__global__ void gpu_compute_flow_properties(unsigned int,double*,double*,double*,double*);

__device__ void taylor_green_eval(unsigned int t, unsigned int x, unsigned int y, double *r, double *u, double *v)
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

__host__ void taylor_green(unsigned int t, double *r, double *u, double *v)
{
    // blocks in grid
    dim3  grid(NX/nThreads, NY, 1);
    // threads in block
    dim3  threads(nThreads, 1, 1);

    gpu_taylor_green<<< grid, threads >>>(t,r,u,v);
    getLastCudaError("gpu_taylor_green kernel error");
}

__global__ void gpu_taylor_green(unsigned int t, double *r, double *u, double *v)
{
    unsigned int y = blockIdx.y;
    unsigned int x = blockIdx.x*blockDim.x+threadIdx.x;
    
    size_t sidx = gpu_scalar_index(x,y);
    
    taylor_green_eval(t,x,y,&r[sidx],&u[sidx],&v[sidx]);
}

__host__ void init_equilibrium(double *f0, double *f1, double *r, double *u, double *v)
{
    // blocks in grid
    dim3  grid(NX/nThreads, NY, 1);
    // threads in block
    dim3  threads(nThreads, 1, 1);

    gpu_init_equilibrium<<< grid, threads >>>(f0,f1,r,u,v);
    getLastCudaError("gpu_init_equilibrium kernel error");
}

__global__ void gpu_init_equilibrium(double *f0, double *f1, double *r, double *u, double *v)
{
    unsigned int y = blockIdx.y;
    unsigned int x = blockIdx.x*blockDim.x+threadIdx.x;
    
    double rho = r[gpu_scalar_index(x,y)];
    double ux  = u[gpu_scalar_index(x,y)];
    double uy  = v[gpu_scalar_index(x,y)];
    
    // load equilibrium
    // feq_i  = w_i rho [1 + 3(ci . u) + (9/2) (ci . u)^2 - (3/2) (u.u)]
    // feq_i  = w_i rho [1 - 3/2 (u.u) + (ci . 3u) + (1/2) (ci . 3u)^2]
    // feq_i  = w_i rho [1 - 3/2 (u.u) + (ci . 3u){ 1 + (1/2) (ci . 3u) }]
    
    // temporary variables
    double w0r = w0*rho;
    double wsr = ws*rho;
    double wdr = wd*rho;
    double omusq = 1.0 - 1.5*(ux*ux+uy*uy);
    
    double tux = 3.0*ux;
    double tuy = 3.0*uy;
    
    f0[gpu_field0_index(x,y)]    = w0r*(omusq);
    
    double cidot3u = tux;
    f1[gpu_fieldn_index(x,y,1)]  = wsr*(omusq + cidot3u*(1.0+0.5*cidot3u));
    cidot3u = tuy;
    f1[gpu_fieldn_index(x,y,2)]  = wsr*(omusq + cidot3u*(1.0+0.5*cidot3u));
    cidot3u = -tux;
    f1[gpu_fieldn_index(x,y,3)]  = wsr*(omusq + cidot3u*(1.0+0.5*cidot3u));
    cidot3u = -tuy;
    f1[gpu_fieldn_index(x,y,4)]  = wsr*(omusq + cidot3u*(1.0+0.5*cidot3u));
    
    cidot3u = tux+tuy;
    f1[gpu_fieldn_index(x,y,5)]  = wdr*(omusq + cidot3u*(1.0+0.5*cidot3u));
    cidot3u = tuy-tux;
    f1[gpu_fieldn_index(x,y,6)]  = wdr*(omusq + cidot3u*(1.0+0.5*cidot3u));
    cidot3u = -(tux+tuy);
    f1[gpu_fieldn_index(x,y,7)]  = wdr*(omusq + cidot3u*(1.0+0.5*cidot3u));
    cidot3u = tux-tuy;
    f1[gpu_fieldn_index(x,y,8)]  = wdr*(omusq + cidot3u*(1.0+0.5*cidot3u));
}

__host__ void stream_collide_save(double *f0, double *f1, double *f2, double *r, double *u, double *v, bool save)
{
    // blocks in grid
    dim3  grid(NX/nThreads, NY, 1);
    // threads in block
    dim3  threads(nThreads, 1, 1);

    gpu_stream_collide_save<<< grid, threads >>>(f0,f1,f2,r,u,v,save);
    getLastCudaError("gpu_stream_collide_save kernel error");
}

__global__ void gpu_stream_collide_save(double *f0, double *f1, double *f2, double *r, double *u, double *v, bool save)
{
    // useful constants
    const double tauinv = 2.0/(6.0*nu+1.0); // 1/tau
    const double omtauinv = 1.0-tauinv;     // 1 - 1/tau

    unsigned int y = blockIdx.y;
    unsigned int x = blockIdx.x*blockDim.x+threadIdx.x;
    
    unsigned int xp1 = (x+1)%NX;
    unsigned int yp1 = (y+1)%NY;
    unsigned int xm1 = (NX+x-1)%NX;
    unsigned int ym1 = (NY+y-1)%NY;
    
    // direction numbering scheme
    // 6 2 5
    // 3 0 1
    // 7 4 8
    
    double ft0 = f0[gpu_field0_index(x,y)];
    
    // load populations from adjacent nodes
    double ft1 = f1[gpu_fieldn_index(xm1,y,  1)];
    double ft2 = f1[gpu_fieldn_index(x,  ym1,2)];
    double ft3 = f1[gpu_fieldn_index(xp1,y,  3)];
    double ft4 = f1[gpu_fieldn_index(x,  yp1,4)];
    double ft5 = f1[gpu_fieldn_index(xm1,ym1,5)];
    double ft6 = f1[gpu_fieldn_index(xp1,ym1,6)];
    double ft7 = f1[gpu_fieldn_index(xp1,yp1,7)];
    double ft8 = f1[gpu_fieldn_index(xm1,yp1,8)];
    
    // compute moments
    double rho = ft0+ft1+ft2+ft3+ft4+ft5+ft6+ft7+ft8;
    double rhoinv = 1.0/rho;
    
    double ux = rhoinv*(ft1+ft5+ft8-(ft3+ft6+ft7));
    double uy = rhoinv*(ft2+ft5+ft6-(ft4+ft7+ft8));
    
    // only write to memory when needed
    if(save)
    {
        r[gpu_scalar_index(x,y)] = rho;
        u[gpu_scalar_index(x,y)] = ux;
        v[gpu_scalar_index(x,y)] = uy;
    }
    
    // now compute and relax to equilibrium
    // note that
    // relax to equilibrium
    // feq_i  = w_i rho [1 + 3(ci . u) + (9/2) (ci . u)^2 - (3/2) (u.u)]
    // feq_i  = w_i rho [1 - 3/2 (u.u) + (ci . 3u) + (1/2) (ci . 3u)^2]
    // feq_i  = w_i rho [1 - 3/2 (u.u) + (ci . 3u){ 1 + (1/2) (ci . 3u) }]
    
    // temporary variables
    double tw0r = tauinv*w0*rho; //   w[0]*rho/tau 
    double twsr = tauinv*ws*rho; // w[1-4]*rho/tau
    double twdr = tauinv*wd*rho; // w[5-8]*rho/tau
    double omusq = 1.0 - 1.5*(ux*ux+uy*uy); // 1-(3/2)u.u
    
    double tux = 3.0*ux;
    double tuy = 3.0*uy;
    
    f0[gpu_field0_index(x,y)]    = omtauinv*ft0  + tw0r*(omusq);
    
    double cidot3u = tux;
    f2[gpu_fieldn_index(x,y,1)]  = omtauinv*ft1  + twsr*(omusq + cidot3u*(1.0+0.5*cidot3u));
    cidot3u = tuy;
    f2[gpu_fieldn_index(x,y,2)]  = omtauinv*ft2  + twsr*(omusq + cidot3u*(1.0+0.5*cidot3u));
    cidot3u = -tux;
    f2[gpu_fieldn_index(x,y,3)]  = omtauinv*ft3  + twsr*(omusq + cidot3u*(1.0+0.5*cidot3u));
    cidot3u = -tuy;
    f2[gpu_fieldn_index(x,y,4)]  = omtauinv*ft4  + twsr*(omusq + cidot3u*(1.0+0.5*cidot3u));
    
    cidot3u = tux+tuy;
    f2[gpu_fieldn_index(x,y,5)]  = omtauinv*ft5  + twdr*(omusq + cidot3u*(1.0+0.5*cidot3u));
    cidot3u = tuy-tux;
    f2[gpu_fieldn_index(x,y,6)]  = omtauinv*ft6  + twdr*(omusq + cidot3u*(1.0+0.5*cidot3u));
    cidot3u = -(tux+tuy);
    f2[gpu_fieldn_index(x,y,7)]  = omtauinv*ft7  + twdr*(omusq + cidot3u*(1.0+0.5*cidot3u));
    cidot3u = tux-tuy;
    f2[gpu_fieldn_index(x,y,8)]  = omtauinv*ft8  + twdr*(omusq + cidot3u*(1.0+0.5*cidot3u));
}

__host__ void compute_flow_properties(unsigned int t, double *r, double *u, double *v,
                             double *prop, double *prop_gpu, double *prop_host)
{
    // prop must point to space for 4 doubles:
    // 0: energy
    // 1: L2 error in rho
    // 2: L2 error in ux
    // 3: L2 error in uy
    
    // blocks in grid
    dim3  grid(NX/nThreads, NY, 1);
    // threads in block
    dim3  threads(nThreads, 1, 1);

    gpu_compute_flow_properties<<< grid, threads, 7*threads.x*sizeof(double) >>>(t,r,u,v,prop_gpu);
    getLastCudaError("gpu_compute_flow_properties kernel error");
    
    // transfer block sums to host memory
    size_t prop_size_bytes = 7*grid.x*grid.y*sizeof(double);
    checkCudaErrors(cudaMemcpy(prop_host,prop_gpu,prop_size_bytes,cudaMemcpyDeviceToHost));
    
    // initialise sums
    double E = 0.0; // kinetic energy
    
    double sumrhoe2 = 0.0; // sum of error squared in rho
    double sumuxe2 = 0.0;  //                         ux
    double sumuye2 = 0.0;  //                         uy
    
    double sumrhoa2 = 0.0; // sum of analytical rho squared
    double sumuxa2 = 0.0;  //                   ux
    double sumuya2 = 0.0;  //                   uy
    
    // finish summation with CPU
    for(unsigned int i = 0; i < grid.x*grid.y; ++i)
    {
        E += prop_host[7*i];
        sumrhoe2 += prop_host[7*i+1];
        sumuxe2  += prop_host[7*i+2];
        sumuye2  += prop_host[7*i+3];

        sumrhoa2 += prop_host[7*i+4];
        sumuxa2  += prop_host[7*i+5];
        sumuya2  += prop_host[7*i+6];
    }
    
    // compute and return final values
    prop[0] = E;
    prop[1] = sqrt(sumrhoe2/sumrhoa2);
    prop[2] = sqrt(sumuxe2/sumuxa2);
    prop[3] = sqrt(sumuye2/sumuya2);
}

__global__ void gpu_compute_flow_properties(unsigned int t, double *r, double *u, double *v, double *prop_gpu)
{
    unsigned int y = blockIdx.y;
    unsigned int x = blockIdx.x*blockDim.x+threadIdx.x;
    
    extern __shared__ double data[];
    
    // set up arrays for each variable
    // each array begins after the previous ends
    double *E     = data;
    double *rhoe2 = data +   blockDim.x;
    double *uxe2  = data + 2*blockDim.x;
    double *uye2  = data + 3*blockDim.x;
    double *rhoa2 = data + 4*blockDim.x;
    double *uxa2  = data + 5*blockDim.x;
    double *uya2  = data + 6*blockDim.x;
    
    // load density and velocity
    double rho = r[gpu_scalar_index(x,y)];
    double ux  = u[gpu_scalar_index(x,y)];
    double uy  = v[gpu_scalar_index(x,y)];
    
    // compute kinetic energy density
    E[threadIdx.x] = rho*(ux*ux + uy*uy);
    
    // compute analytical results
    double rhoa, uxa, uya;
    taylor_green_eval(t,x,y,&rhoa,&uxa,&uya);
    
    // compute terms for L2 error
    rhoe2[threadIdx.x] = (rho-rhoa)*(rho-rhoa);
    uxe2[threadIdx.x]  = (ux-uxa)*(ux-uxa);
    uye2[threadIdx.x]  = (uy-uya)*(uy-uya);
    
    rhoa2[threadIdx.x] = (rhoa-rho0)*(rhoa-rho0);
    uxa2[threadIdx.x]  = uxa*uxa;
    uya2[threadIdx.x]  = uya*uya;
    
    // synchronise data in shared memory
    __syncthreads();
    
    // only one thread proceeds
    if(threadIdx.x == 0)
    {
        // compute linear index for this block within grid
        size_t idx = 7*(gridDim.x*blockIdx.y+blockIdx.x);
        
        for(int n = 0; n  < 7; ++n)
            prop_gpu[idx+n] = 0.0;
        
        // sum values for this block from shared memory
        for(int i = 0; i < blockDim.x; ++i)
        {
            prop_gpu[idx  ] += E[i];
            prop_gpu[idx+1] += rhoe2[i];
            prop_gpu[idx+2] += uxe2[i];
            prop_gpu[idx+3] += uye2[i];

            prop_gpu[idx+4] += rhoa2[i];
            prop_gpu[idx+5] += uxa2[i];
            prop_gpu[idx+6] += uya2[i];
        }
    }
}

__host__ void report_flow_properties(unsigned int t, double *rho, double *ux, double *uy,
                                     double *prop_gpu, double *prop_host)
{
    double prop[4];
    compute_flow_properties(t,rho,ux,uy,prop,prop_gpu,prop_host);
    printf("%u,%g,%g,%g,%g\n",t,prop[0],prop[1],prop[2],prop[3]);
}

__host__ void save_scalar(const char* name, double *scalar_gpu, double *scalar_host, unsigned int n)
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
    
    // transfer memory from GPU to host
    checkCudaErrors(cudaMemcpy(scalar_host,scalar_gpu,mem_size_scalar,cudaMemcpyDeviceToHost));
    
    // open file for writing
    FILE *fout = fopen(filename,"wb+");
    
    // write data
    fwrite(scalar_host,1,mem_size_scalar,fout);
    
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

