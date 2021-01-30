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

#include <mpi.h>

#include "seconds.h"
#include "LBM.h"

int main(int argc, char* argv[])
{
    int rank, nprocs;
    
    // initialize MPI
    MPI_Init(&argc,&argv);
    
    // save rank of this process
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);
    
    // save number of processes
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    
    if(rank == 0)
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
        printf("MPI information\n");
        printf("        processes: %d\n",nprocs);
        printf("\n");
    }
    
    MPI_Barrier(MPI_COMM_WORLD);
    
    unsigned int rank_ystart;
    unsigned int rank_ny;
    
    if(rank < NY % nprocs) // ranks that are less than remainder
    {
        rank_ny = NY/nprocs+1;
        rank_ystart = rank*rank_ny;
    }
    else
    {
        rank_ny = NY/nprocs;
        rank_ystart = NY-(nprocs-rank)*rank_ny;
    }
    printf("Rank %d: %d nodes from y = %d to y = %d\n",rank,rank_ny,rank_ystart,rank_ystart+rank_ny-1);
    
    double bytesPerMiB = 1024.0*1024.0;
    double bytesPerGiB = 1024.0*1024.0*1024.0;
    
    size_t mem_size_0dir   = sizeof(double)*NX*rank_ny;
    size_t mem_size_n0dir  = sizeof(double)*NX*(rank_ny+2)*(ndir-1);
    size_t mem_size_scalar = sizeof(double)*NX*rank_ny;

    double *f0  = (double*) malloc(mem_size_0dir);
    double *f1  = (double*) malloc(mem_size_n0dir);
    double *f2  = (double*) malloc(mem_size_n0dir);
    double *rho = (double*) malloc(mem_size_scalar);
    double *ux  = (double*) malloc(mem_size_scalar);
    double *uy  = (double*) malloc(mem_size_scalar);
    
    size_t total_mem_bytes = mem_size_0dir + 2*mem_size_n0dir + 3*mem_size_scalar;
    
    if(f0 == NULL || f1 == NULL || f2 == NULL || rho == NULL || ux == NULL || uy == NULL)
    {
        fprintf(stderr,"Error: unable to allocate required memory (%.1f MiB).\n",total_mem_bytes/bytesPerMiB);
        exit(-1);
    }
    
    // compute Taylor-Green flow at t=0 
    // to initialise rho, ux, uy fields.
    taylor_green(0,rho,ux,uy,rank_ystart,rank_ny);
    
    // initialise f1 as equilibrium for rho, ux, uy
    init_equilibrium(f0,f1,rho,ux,uy,rank_ny);
    
    save_scalar("rho",rho,0,rank,mem_size_scalar);
    save_scalar("ux", ux, 0,rank,mem_size_scalar);
    save_scalar("uy", uy, 0,rank,mem_size_scalar);
    if(computeFlowProperties)
    {
        report_flow_properties(0,rho,ux,uy,rank,rank_ystart,rank_ny);
    }

    // prepare some constants for MPI transfers
    int rankp1 = (rank+1) % nprocs;
    int rankm1 = (nprocs+rank-1) % nprocs;
    size_t transfer_doubles = (ndir-1)*NX;
    MPI_Request reqs[4];
    MPI_Status  stats[4];

    MPI_Barrier(MPI_COMM_WORLD);
    
    double start = seconds();
    
    // main simulation loop; take NSTEPS time steps
    for(unsigned int n = 0; n < NSTEPS; ++n)
    {
        bool save = (n+1)%NSAVE == 0;
        bool msg  = (n+1)%NMSG == 0;
        bool need_scalars = save || (msg && computeFlowProperties);
        
        // initiate data transfers
        MPI_Isend(&f1[fieldn_index(0,rank_ny-1,1)],transfer_doubles,MPI_DOUBLE,rankp1,rank,  MPI_COMM_WORLD,&reqs[0]);
        MPI_Irecv(&f1[fieldn_index(0,-1,1)],transfer_doubles,MPI_DOUBLE,rankm1,rankm1,MPI_COMM_WORLD,&reqs[1]);
        
        MPI_Isend(&f1[fieldn_index(0,0,1)],transfer_doubles,MPI_DOUBLE,rankm1,rank,  MPI_COMM_WORLD,&reqs[2]);
        MPI_Irecv(&f1[fieldn_index(0,rank_ny,1)],transfer_doubles,MPI_DOUBLE,rankp1,rankp1,MPI_COMM_WORLD,&reqs[3]);
        
        // stream and collide from f1 storing to f2
        // optionally compute and save moments
        // only update internal rows and ensure comms progress
        stream_collide_save_test(f0,f1,f2,rho,ux,uy,need_scalars,1,rank_ny-1,4,reqs,stats);

        // wait for transfers to finish
        MPI_Waitall(4,reqs,stats);
        
        // stream and collide first and last rows
        stream_collide_save(f0,f1,f2,rho,ux,uy,need_scalars,0,1);
        stream_collide_save(f0,f1,f2,rho,ux,uy,need_scalars,rank_ny-1,rank_ny);
        
        if(save)
        {
            save_scalar("rho",rho,n+1,rank,mem_size_scalar);
            save_scalar("ux", ux, n+1,rank,mem_size_scalar);
            save_scalar("uy", uy, n+1,rank,mem_size_scalar);
        }
        
        // swap pointers
        double *temp = f1;
        f1 = f2;
        f2 = temp;
        
        if(msg)
        {
            if(computeFlowProperties)
            {
                report_flow_properties(n+1,rho,ux,uy,rank,rank_ystart,rank_ny);
            }
            
            if(rank == 0 && !quiet)
                printf("completed timestep %d\n",n+1);
        }
    }
    
    MPI_Barrier(MPI_COMM_WORLD);
    
    double end = seconds();
    double runtime = end-start;

    size_t doubles_read = ndir; // per node every time step
    size_t doubles_written = ndir;
    size_t doubles_saved = 3; // per node every NSAVE time steps
    
    // note NX*NY overflows when NX=NY=65536
    size_t nodes_updated = NSTEPS*size_t(NX*NY);
    size_t nodes_saved   = (NSTEPS/NSAVE)*size_t(NX*NY);
    double speed = nodes_updated/(1e6*runtime);
    
    double bandwidth = (nodes_updated*(doubles_read + doubles_written)+nodes_saved*(doubles_saved))*sizeof(double)/(runtime*bytesPerGiB);
    
    size_t global_total_mem_bytes = 0;
    MPI_Reduce(&total_mem_bytes,&global_total_mem_bytes,1,MPI_LONG_LONG_INT,MPI_SUM,0,MPI_COMM_WORLD);
    
    if(rank == 0)
    {
        printf(" ----- performance information -----\n");
        printf("memory allocated: %.1f (MiB)\n",global_total_mem_bytes/bytesPerMiB);
        printf("       timesteps: %u\n",NSTEPS);
        printf("         runtime: %.3f (s)\n",runtime);
        printf("           speed: %.2f (Mlups)\n",speed);
        printf("       bandwidth: %.1f (GiB/s)\n",bandwidth);
    }
    
    // deallocate memory
    free(f0);  free(f1); free(f2);
    free(rho); free(ux); free(uy);
    
    MPI_Finalize();
    
    return 0;
}

