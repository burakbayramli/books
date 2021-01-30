/* This code accompanies
 *   The Lattice Boltzmann Method: Principles and Practice
 *   T. Krüger, H. Kusumaatmaja, A. Kuzmin, O. Shardt, G. Silva, E.M. Viggen
 *   ISBN 978-3-319-44649-3 (Electronic) 
 *        978-3-319-44647-9 (Print)
 *   http://www.springer.com/978-3-319-44647-9
 *
 * This code is provided under the MIT license. See LICENSE.txt.
 *
 * Author: Alexandr Kuzmin
 * 
 */
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <stdio.h>
#include <stdlib.h>
#include <cmath>
#include <vector>

//Domain size
int NY;
int NX;
int NUM;

//Other constants
const int NPOP=9;

//Time steps
int N=50000;
int NOUTPUT=100;

//Fields and populations
double *f;
double *f2;
double *rho;
double *ux;
double *uy;
int * geometry;

//Boundary conditions
double conc_wall=1.0;
double conc_inlet=0.5;
double u0=0.05;
std::vector<int> bb_nodes_inlet;
std::vector<char>* dirs_inlet;

std::vector<int> bb_nodes_wall;
std::vector<int> dirs_wall;

//BGK relaxation parameter
double omega=1.4; ///1.99;
double omega_plus=2.0-omega;
double omega_minus=omega;

//Diffusion parameter
double ce=1.0/3.0;

//Underlying lattice parameters
double weights[]={4.0/9.0,1.0/9.0,1.0/9.0,1.0/9.0,1.0/9.0,1.0/36.0,1.0/36.0,1.0/36.0,1.0/36.0};
double weights_trt[]={0.0,1.0/3.0,1.0/3.0,1.0/3.0,1.0/3.0,1.0/12.0,1.0/12.0,1.0/12.0,1.0/12.0};
int cx[]={0,1,0,-1,0,1,-1,-1,1};
int cy[]={0,0,1,0,-1,1,1,-1,-1};
int complement[]={0,3,4,1,2,7,8,5,6};
int pxx[]={0, 1, -1, 1, -1, 0, 0, 0, 0};
int pxy[]={0, 0, 0, 0, 0, 1, -1, 1, -1};
void writedensity(std::string const & fname)
{
    std::string filename = "film_inamuro/" + fname + ".dat";
    std::ofstream fout(filename.c_str());
    fout.precision(10);

    for (int iX=0; iX<NX; ++iX)
    {
        for (int iY=0; iY<NY; iY++)
        {
            int counter=iY*NX+iX;
            fout<<rho[counter]<<" ";
        }
        fout<<"\n";
    }
}

void writevelocityx(std::string const & fname)
{
    std::string filename = "film_inamuro/" + fname + ".dat";
    std::ofstream fout(filename.c_str());
    fout.precision(10);

    for (int iX=0; iX<NX; ++iX)
    {
        for (int iY=0; iY<NY; iY++)
        {
            int counter=iY*NX+iX;
            fout<<ux[counter]<<" ";
        }
        fout<<"\n";
    }
}

void writevelocityy(std::string const & fname)
{
    std::string filename = "film_inamuro/" + fname + ".dat";
    std::ofstream fout(filename.c_str());
    fout.precision(10);

    for (int iX=0; iX<NX; ++iX)
    {
        for (int iY=0; iY<NY; iY++)
        {
            int counter=iY*NX+iX;
            fout<<uy[counter]<<" ";
        }
        fout<<"\n";
    }
}






void collide_bgk()
{
    for (int iY=0;iY<NY;iY++)
        for(int iX=0;iX<NX;iX++)
        {
            int counter=iY*NX+iX;
            rho[counter]=0.0;
            
            int offset=counter*NPOP;
     
            double sum=0;
            for(int k=0;k<NPOP;k++)
                sum+=f[offset+k];
    
            rho[counter]=sum;

            double dense_temp=rho[counter];
            double ux_temp=ux[counter];
            double uy_temp=uy[counter];
            
            double feq[NPOP];
     
            //Collision operator
            for(int k=0; k < NPOP; k++)
            {
                feq[k]=weights[k]*dense_temp*(1.0+3.0*(cx[k]*ux_temp+cy[k]*uy_temp)+4.5*((cx[k]*cx[k]-1.0/3.0)*ux_temp*ux_temp+2.0*cx[k]*cy[k]*ux_temp*uy_temp+(cy[k]*cy[k]-1.0/3.0)*uy_temp*uy_temp));
                f2[offset+k]=f[offset+k]-omega*(f[offset+k]-feq[k]);
            }
    }
            
}


void update_bounce_back()
{
    //Updating inlet populations
    for(int iX=1;iX<NX-1;iX++)
    {
        int offset=iX*NPOP;
        double dense_temp;
        
        dense_temp=(conc_inlet-(f[offset+1]+f[offset+3]+f[offset+4]+f[offset+7]+f[offset+8]))/(weights[2]+weights[5]+weights[6]);    
        f2[offset+2]=weights[2]*dense_temp;
        f2[offset+5]=weights[5]*dense_temp;
        f2[offset+6]=weights[6]*dense_temp;
    }

    //Updating outlet populations
    for(int iX=1;iX<NX-1;iX++)
    {
        int offset=((NY-1)*NX+iX)*NPOP;
        double dense_temp;
        
        dense_temp=(f[offset+2]+f[offset+5]+f[offset+6])/(weights[4]+weights[7]+weights[8]);    
        f2[offset+4]=weights[4]*dense_temp;
        f2[offset+7]=weights[7]*dense_temp;
        f2[offset+8]=weights[8]*dense_temp;
    }
    
    //Updating wall populations
    for(int iY=1;iY<NY-1;iY++)
    {
        int offset=iY*NX*NPOP;
        double dense_temp;
        
        dense_temp=(conc_wall-(f[offset+2]+f[offset+3]+f[offset+4]+f[offset+6]+f[offset+7]))/(weights[1]+weights[5]+weights[8]);    
        f2[offset+1]=weights[1]*dense_temp;
        f2[offset+5]=weights[5]*dense_temp;
        f2[offset+8]=weights[8]*dense_temp;
    }

    //Updating free-wall populations
    for(int iY=1;iY<NY-1;iY++)
    {
        int offset=(iY*NX+NX-1)*NPOP;
        double dense_temp;
        
        dense_temp=(f[offset+1]+f[offset+5]+f[offset+8])/(weights[6]+weights[3]+weights[7]);    
        f2[offset+6]=weights[6]*dense_temp;
        f2[offset+3]=weights[3]*dense_temp;
        f2[offset+7]=weights[7]*dense_temp;
    }

    

    //Corners (averaging procedure)
    for(int iPop=0;iPop<NPOP;iPop++)
    {
        f2[iPop]            =0.5*(f2[NPOP+iPop]+f2[NX*NPOP+iPop]);
        f2[(NX-1)*NPOP+iPop]=0.5*(f2[(NX-2)*NPOP+iPop]+f2[(2*NX-1)*NPOP+iPop]);
        f2[NX*(NY-1)*NPOP+iPop]=0.5*(f2[NX*(NY-2)*NPOP+iPop]+f2[(NX*(NY-1)+1)*NPOP+iPop]);
        f2[(NX*(NY-1)+NX-1)*NPOP+iPop]=0.5*(f2[(NX*(NY-2)+NX-1)*NPOP+iPop]+f2[(NX*(NY-1)+NX-2)*NPOP+iPop]);
    }
        
}


void update_bounce_back_after_stream()
{
    //Updating inlet populations
    for(int iX=1;iX<NX-1;iX++)
    {
        int offset=iX*NPOP;
        double dense_temp;
        
        dense_temp=(conc_inlet-(f[offset+0]+f[offset+1]+f[offset+3]+f[offset+4]+f[offset+7]+f[offset+8]))/(weights[2]+weights[5]+weights[6]);    
        f[offset+2]=weights[2]*dense_temp;
        f[offset+5]=weights[5]*dense_temp;
        f[offset+6]=weights[6]*dense_temp;
    }

    //Updating outlet populations
    for(int iX=1;iX<NX-1;iX++)
    {
        int offset=((NY-1)*NX+iX)*NPOP;
        double dense_temp;
        
        dense_temp=(f[offset+2]+f[offset+5]+f[offset+6])/(weights[4]+weights[7]+weights[8]);    
        f[offset+4]=weights[4]*dense_temp;
        f[offset+7]=weights[7]*dense_temp;
        f[offset+8]=weights[8]*dense_temp;
    }
    
    //Updating wall populations
    for(int iY=1;iY<NY-1;iY++)
    {
        int offset=iY*NX*NPOP;
        double dense_temp;
        
        dense_temp=(conc_wall-(f[offset+0]+f[offset+2]+f[offset+3]+f[offset+4]+f[offset+6]+f[offset+7]))/(weights[1]+weights[5]+weights[8]);    
        f[offset+1]=weights[1]*dense_temp;
        f[offset+5]=weights[5]*dense_temp;
        f[offset+8]=weights[8]*dense_temp;
    }

    //Updating free-wall populations
    for(int iY=1;iY<NY-1;iY++)
    {
        int offset=(iY*NX+NX-1)*NPOP;
        double dense_temp;
        
        dense_temp=(f[offset+1]+f[offset+5]+f[offset+8])/(weights[6]+weights[3]+weights[7]);    
        f[offset+6]=weights[6]*dense_temp;
        f[offset+3]=weights[3]*dense_temp;
        f[offset+7]=weights[7]*dense_temp;
    }

    

    //Corners (averaging procedure)
    for(int iPop=0;iPop<NPOP;iPop++)
    {
        f[iPop]            =0.5*(f[NPOP+iPop]+f[NX*NPOP+iPop]);
        f[(NX-1)*NPOP+iPop]=0.5*(f[(NX-2)*NPOP+iPop]+f[(2*NX-1)*NPOP+iPop]);
        f[NX*(NY-1)*NPOP+iPop]=0.5*(f[NX*(NY-2)*NPOP+iPop]+f[(NX*(NY-1)+1)*NPOP+iPop]);
        f[(NX*(NY-1)+NX-1)*NPOP+iPop]=0.5*(f[(NX*(NY-2)+NX-1)*NPOP+iPop]+f[(NX*(NY-1)+NX-2)*NPOP+iPop]);
    }
        
}



void initialize_geometry()
{
    NY=10*40;
    NX=40;
    NUM=NX*NY;
    geometry=new int[NUM];
    rho=new double[NUM];
    ux=new double[NUM];
    uy=new double[NUM];
    
 
    for(int iY=0;iY<NY;iY++)
        for(int iX=0;iX<NX;iX++)
        {
            int counter=iY*NX+iX;
            rho[counter]=0.0;
            uy[counter]=u0*(1.0-double(iX*iX)/((NX-1.5)*(NX-1.5)));
            ux[counter]=0.0;
        }
 
    for(int iX=1;iX<NX-1;iX++)
    { 
        rho[iX]=conc_inlet;
    }
    
    for(int iY=0;iY<NY;iY++)
    {
        rho[iY*NX]=conc_wall;
    }

    writedensity("conc_initial");
    writevelocityx("ux_initial");
    writevelocityy("uy_initial");    
}

void init()
{
    //Creating arrays
    f=new double[NUM*NPOP];
    f2=new double[NUM*NPOP];
    
    //Bulk nodes initialization
    double feq;
    
    for(int iY=0;iY<NY;iY++)
        for(int iX=0;iX<NX;iX++)
        {
            int  counter=iY*NX+iX;
            double dense_temp=rho[counter];
            double ux_temp=ux[counter];
            double uy_temp=uy[counter];
            
            for (int k=0; k<NPOP; k++)
            {
                feq=weights[k]*(dense_temp+3.0*dense_temp*(cx[k]*ux_temp+cy[k]*uy_temp)
                                +4.5*dense_temp*((cx[k]*cx[k]-1.0/3.0)*ux_temp*ux_temp
                                                +(cy[k]*cy[k]-1.0/3.0)*uy_temp*uy_temp
                                                +2.0*ux_temp*uy_temp*cx[k]*cy[k]));
                f[counter*NPOP+k]=feq;
            }
            
        }

}



void finish_simulation()
{
    delete[] geometry;
    delete[] rho;
    delete[] ux;
    delete[] uy;
    delete[] f;
    delete[] f2;
}


void stream()
{
    for (int iY=0;iY<NY;iY++)
        for(int iX=0;iX<NX;iX++)
        {
            int counter=iY*NX+iX;
            for(int iPop=0;iPop<NPOP;iPop++)
            {
                int iX2=(iX+cx[iPop]+NX)%NX;
                int iY2=(iY+cy[iPop]+NY)%NY;
                int counter2=iY2*NX+iX2;
                f[counter2*NPOP+iPop]=f2[counter*NPOP+iPop];
            }
        }
}


int main(int argc, char* argv[])
{
    initialize_geometry();
    
    init();

    for(int counter=0;counter<=N;counter++)
    {
    
        collide_bgk();
        stream();
        update_bounce_back_after_stream();
        
        //Writing files
        if (counter%NOUTPUT==0)
        {
            std::cout<<"Counter="<<counter<<"\n";
            std::stringstream filewritedensity;
             
            std::stringstream counterconvert;
            counterconvert<<counter;
            filewritedensity<<std::fixed;

            filewritedensity<<"film_inamuro"<<std::string(7-counterconvert.str().size(),'0')<<counter;
            
            writedensity(filewritedensity.str());
        }

    }

    finish_simulation();
    
    return 0;
}
