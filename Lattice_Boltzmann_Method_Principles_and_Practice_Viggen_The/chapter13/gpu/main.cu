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

#include "seconds.h"
#include "LBM.cu"

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
    double bytesPerGiB = 1024.0*1024.0*1024.0;
    
    checkCudaErrors(cudaSetDevice(0));
    int deviceId = 0;
    checkCudaErrors(cudaGetDevice(&deviceId));
    
    cudaDeviceProp deviceProp;
    checkCudaErrors(cudaGetDeviceProperties(&deviceProp, deviceId));
    
    size_t gpu_free_mem, gpu_total_mem;
    checkCudaErrors(cudaMemGetInfo(&gpu_free_mem,&gpu_total_mem));
    
    printf("CUDA information\n");
    printf("       using device: %d\n", deviceId);
    printf("               name: %s\n",deviceProp.name);
    printf("    multiprocessors: %d\n",deviceProp.multiProcessorCount);
    printf(" compute capability: %d.%d\n",deviceProp.major,deviceProp.minor);
    printf("      global memory: %.1f MiB\n",deviceProp.totalGlobalMem/bytesPerMiB);
    printf("        free memory: %.1f MiB\n",gpu_free_mem/bytesPerMiB);
    printf("\n");
    
    double *f0_gpu,*f1_gpu,*f2_gpu;
    double *rho_gpu,*ux_gpu,*uy_gpu;
    double *prop_gpu;
    checkCudaErrors(cudaMalloc((void**)&f0_gpu,mem_size_0dir));
    checkCudaErrors(cudaMalloc((void**)&f1_gpu,mem_size_n0dir));
    checkCudaErrors(cudaMalloc((void**)&f2_gpu,mem_size_n0dir));
    checkCudaErrors(cudaMalloc((void**)&rho_gpu,mem_size_scalar));
    checkCudaErrors(cudaMalloc((void**)&ux_gpu,mem_size_scalar));
    checkCudaErrors(cudaMalloc((void**)&uy_gpu,mem_size_scalar));
    const size_t mem_size_props = 7*NX/nThreads*NY*sizeof(double);
    checkCudaErrors(cudaMalloc((void**)&prop_gpu,mem_size_props));
    
    double *scalar_host  = (double*) malloc(mem_size_scalar);
    if(scalar_host == NULL)
    {
        fprintf(stderr,"Error: unable to allocate required host memory (%.1f MiB).\n",mem_size_scalar/bytesPerMiB);
        exit(-1);
    }
    
    size_t total_mem_bytes = mem_size_0dir + 2*mem_size_n0dir + 3*mem_size_scalar + mem_size_props;

    // create event objects
    cudaEvent_t start, stop;
    checkCudaErrors(cudaEventCreate(&start));
    checkCudaErrors(cudaEventCreate(&stop));
    
    // compute Taylor-Green flow at t=0 
    // to initialise rho, ux, uy fields.
    taylor_green(0,rho_gpu,ux_gpu,uy_gpu);
    
    // initialise f1 as equilibrium for rho, ux, uy
    init_equilibrium(f0_gpu,f1_gpu,rho_gpu,ux_gpu,uy_gpu);
    
    save_scalar("rho",rho_gpu,scalar_host,0);
    save_scalar("ux", ux_gpu, scalar_host,0);
    save_scalar("uy", uy_gpu, scalar_host,0);
    
    if(computeFlowProperties)
    {
        report_flow_properties(0,rho_gpu,ux_gpu,uy_gpu,prop_gpu,scalar_host);
    }
    
    double begin = seconds();
    checkCudaErrors(cudaEventRecord(start,0));
    
    // main simulation loop; take NSTEPS time steps
    for(unsigned int n = 0; n < NSTEPS; ++n)
    {
        bool save = (n+1)%NSAVE == 0;
        bool msg  = (n+1)%NMSG == 0;
        bool need_scalars = save || (msg && computeFlowProperties);
        
        // stream and collide from f1 storing to f2
        // optionally compute and save moments
        stream_collide_save(f0_gpu,f1_gpu,f2_gpu,rho_gpu,ux_gpu,uy_gpu,need_scalars);
        
        if(save)
        {
            save_scalar("rho",rho_gpu,scalar_host,n+1);
            save_scalar("ux", ux_gpu, scalar_host,n+1);
            save_scalar("uy", uy_gpu, scalar_host,n+1);
        }
        
        // swap pointers
        double *temp = f1_gpu;
        f1_gpu = f2_gpu;
        f2_gpu = temp;
        
        if(msg)
        {
            if(computeFlowProperties)
            {
                // note: scalar_host is big enough by a factor of nThreads/7
                report_flow_properties(n+1,rho_gpu,ux_gpu,uy_gpu,prop_gpu,scalar_host);
            }
            
            if(!quiet)
                printf("completed timestep %d\n",n+1);
        }
    }
    checkCudaErrors(cudaEventRecord(stop,0));
    checkCudaErrors(cudaEventSynchronize(stop));
    float milliseconds = 0.0f;
    checkCudaErrors(cudaEventElapsedTime(&milliseconds,start,stop));
    
    double end = seconds();
    double runtime = end-begin;
    double gpu_runtime = 0.001*milliseconds;

    size_t doubles_read = ndir; // per node every time step
    size_t doubles_written = ndir;
    size_t doubles_saved = 3; // per node every NSAVE time steps
    
    // note NX*NY overflows when NX=NY=65536
    size_t nodes_updated = NSTEPS*size_t(NX*NY);
    size_t nodes_saved   = (NSTEPS/NSAVE)*size_t(NX*NY);
    double speed = nodes_updated/(1e6*runtime);
    
    double bandwidth = (nodes_updated*(doubles_read + doubles_written)+nodes_saved*(doubles_saved))*sizeof(double)/(runtime*bytesPerGiB);
    
    printf(" ----- performance information -----\n");
    printf("  memory allocated (GPU): %.1f (MiB)\n",total_mem_bytes/bytesPerMiB);
    printf(" memory allocated (host): %.1f (MiB)\n",mem_size_scalar/bytesPerMiB);
    printf("               timesteps: %u\n",NSTEPS);
    printf("           clock runtime: %.3f (s)\n",runtime);
    printf("             gpu runtime: %.3f (s)\n",gpu_runtime);
    printf("                   speed: %.2f (Mlups)\n",speed);
    printf("               bandwidth: %.1f (GiB/s)\n",bandwidth);
    
    // destory event objects
    checkCudaErrors(cudaEventDestroy(start));
    checkCudaErrors(cudaEventDestroy(stop));
    
    // free all memory allocatd on the GPU and host
    checkCudaErrors(cudaFree(f0_gpu));
    checkCudaErrors(cudaFree(f1_gpu));
    checkCudaErrors(cudaFree(f2_gpu));
    checkCudaErrors(cudaFree(rho_gpu));
    checkCudaErrors(cudaFree(ux_gpu));
    checkCudaErrors(cudaFree(uy_gpu));
    checkCudaErrors(cudaFree(prop_gpu));    
    free(scalar_host);
    
    // release resources associated with the GPU device
    cudaDeviceReset();
    
    return 0;
}

