/* 

$Author$
$Log$
$Revision$

*/

#include <stdio.h>
#include <math.h>
#include <limits.h> 
#include "macros_gen.h"
#include "macros_6.h" 
#include "parameters.h" 

#define NZMAX 10000000
#define RAND1 (float) ran1(&seed)
long seed;


void gravitate(g,flag, nx, ny, solid, lattice )
     float g;
     char flag;
     int nx, ny;
     unsigned char *solid, *lattice;
{
/*---------------------------------------------------------------*
 * Gravity:
 * flag can be 'i', meaning "initialize;" in that case, various
 * static variables are initialized; or 'f', meaning "force."
 * nx, ny dimsion of lattice
 * solid, lattice are selfexplanatory
 *---------------------------------------------------------------*/  

  float ran1();
  static int states, gravitons;
  static float dgextra, gextra;

  switch (flag)
    {
    case 'i':  /*initialize*/
      {
	int i, j, size;
	states = 0;
	size = nx*ny;
      
	for (i=0; i<size; i++)
	  if (!(solid[i] & 1))
	    states += PARTSUM(lattice[i]);

	gravitons = (int) (g/2.0 * (float) (states));
	dgextra = g/2.0 * (float) (states) - (float) gravitons;
	gextra=0.0;
      }

      break;
    case 'f': /*force*/
      {
	int flip, flips, i, choice, size;

	size = nx*ny;
      
	gextra += dgextra;
	flips = gravitons + (int)gextra;
	gextra -= (int)gextra;
	for (flip=0;flip < flips;)
	  {
	    choice=(int) (RAND1*size);
	    if (!(solid[choice] & 1))
	      {
		if( (DP(lattice[choice])) && !(AP(lattice[choice])) )
		  {
		    lattice[choice] &= (~D);
		    lattice[choice] |= A;
		    flip++;
		  }
	      }	  
	  }
      }
      break;
    }/*end of switch */
  
}

/*float ran1(long*);*/
void main()
{
  int nx,ny,nz,ix,iy,k,iz;
  int jb,jc,je,jf;
  int za,zb,zc,zd,ze,zf;
  int px,py,npart;
  int t, particle, gravity;
  unsigned char conf, eps;
  unsigned char *latt, *newlatt, *solid, *table, *ptr;
  float dens, g;
  FILE  *fpxmom, *fpymom, *fpmass, *solidfd, *fPtrSolid;
  int *xmom, *ymom, *mass;
  float ran1();
  float dR, phi;
  int iAngle,nAngles, iR;


  nx=NX;
  ny=NY;
  dens=DENSITY;
  seed  -= (long) time(0);                             
  nz=nx*ny;

  if(nz > NZMAX) 
    {
      fprintf(stderr,"nz too large\n");
      exit(1);
    }

  if(!(latt = (unsigned char *) malloc(nz*sizeof(unsigned char))))
    {
      fprintf(stderr,"%s:%d allocation error\n",__FILE__,__LINE__);
      exit(-1);
    }
  if(!(newlatt = (unsigned char *) malloc(nz*sizeof(unsigned char))))
    {
      fprintf(stderr,"%s:%d allocation error\n",__FILE__,__LINE__);      
      exit(-1);
    }
  if(!(table =   (unsigned char *) malloc(256*sizeof(unsigned char))))
    {
      fprintf(stderr,"%s:%d allocation error\n",__FILE__,__LINE__);      
      exit(-1);
    }      

  if(!(solid =   (unsigned char *) malloc(nz*sizeof(unsigned char))))
    {
      fprintf(stderr,"%s:%d allocation error\n",__FILE__,__LINE__);      
      exit(-1);
    }

  if(!(xmom=(int *) calloc(nz,sizeof(int))))
    {
      fprintf(stderr,"%s:%d allocation error\n",__FILE__,__LINE__);      
      exit(-1);
    }
   
  if(!(ymom=(int *) calloc(nz,sizeof(int))))
    {
      fprintf(stderr,"%s:%d allocation error\n",__FILE__,__LINE__);      
      exit(-1);
    }
  if(!(mass=(int *) calloc(nz,sizeof(int))))
    {
      fprintf(stderr,"%s:%d allocation error\n",__FILE__,__LINE__);      
      exit(-1);
    }

   


  tabgen(table); 


/* Initialize the lattice with density dens  */


  for(iz=0;iz<nz;iz++)
    {
      latt[iz] = 0;
    }

  for(iz=0;iz<nz;iz++)
    {
      for(k=0; k<6; k++)
      {
	particle= (int) (RAND1 + dens);   
	if(particle)
	  {
	    latt[iz] |= (1 << k);
	  }
      }
    }

/* Read a file containing data about solid sites */

  
	if((solidfd = fopen("solid","r")) != NULL) 
	  {
	    iz=fread((char *) solid, sizeof(char), nz*sizeof(char),solidfd);
	  }
	else
	  {
	    fprintf(stderr,"No solid site configuration found\n");
	    fprintf(stderr,"Continuing normal operation without solid sites\n");
	    for(iz=0;iz<nz;iz++)
	      solid[iz]=0;
	  }

/* open files for x, y momenum */
  if((fpxmom = fopen("x_mom","w")) == NULL) 
    {
      fprintf(stderr,"%s:%d cannot open file x_mom\n",__FILE__,__LINE__);
      exit(-1);
    }
  if((fpymom = fopen("y_mom","w")) == NULL) 
    {
      fprintf(stderr,"%s:%d cannot open file y_mom\n",__FILE__,__LINE__);
      exit(-1);
    }
  if((fpmass = fopen("mass","w")) == NULL) 
    {
      fprintf(stderr,"%s:%d cannot open file mass\n",__FILE__,__LINE__);
      exit(-1);
    }


#if PARABOLAS
/* Add walls */

  for (ix=0; ix< nx; ix++)
    for(iy=0; iy< ny ; iy++) 
      {
	iz= ix + nx*iy;
	solid[iz] = 0 ;
	if((iy< WALL_THICKNESS) || (iy> ny - WALL_THICKNESS -1) ) 
	    solid[iz] = 1;
      }
#endif


#if DISK
 fprintf(stderr,"disk with radius: %d at x=%d, y=%d\n",
	  RADIUS,XC,YC); 

/* opening file with solid */
  for(iR=1;iR<1*RADIUS;iR++)
    {
      dR=(float) iR/1.;
      nAngles=(int) (2*PI*(RADIUS));
      nAngles *= 20;
      for(iAngle=1;iAngle<nAngles;iAngle++)
	{
	  phi=(float) iAngle/(nAngles+1);
	  ix=(int) dR*cos(phi*2*PI)+XC;
	  iy=(int) dR*sin(phi*2*PI)+YC;
	  *(solid+iy*nx+ix) = 1;
	}
    }
  fPtrSolid=fopen("disksolid","w");
    for(iy=0;iy<ny;iy++)
      {
	for(ix=0;ix<nx;ix++)
	  fprintf(fPtrSolid,"%d ",solid[iy*nx+ix]);
	fprintf(fPtrSolid,"\n");
      }
#endif



/* add solid to the lattice */
  for(iz=0;iz<nz;iz++)
    {
      if(solid[iz]) latt[iz] |= S; 
    }

  fprintf(stderr,"initial state:\n");
  fprintf(stderr,"-------------------------------------\n");
  test_cons(latt,&nz,&px,&py,&npart); 

#ifdef FORCING_RATE
/* initialize forcing */
  g=FORCING_RATE;
  gravitate(g,'i',nx,ny,solid,latt);
#endif

  for(t=0;t<TMAX+TAVG;t++)
    {
      for(iz=0;iz<nz;iz++)
	newlatt[iz] = 0;

#ifdef FORCING_RATE
      gravitate(g,'f',nx,ny,solid,latt);
#endif

/* Adjust the ``random'' bit */

      if(t&1) 
	{
	  eps = EPS;
	}
      else
	{
	  eps = 0;
	}

      /* Loop over y-axis (slow dimension) */
      for(iy=0;iy<ny;iy++)
	{
	  jb = WRAP_AROUND(iy+1,ny);
	  jc = jb;
	  je = WRAP_AROUND(iy-1,ny);
	  jf = je;
	  for(ix=0;ix<nx;ix++)
	    {
	      iz  = ix+ nx*iy;
	      za = WRAP_AROUND(ix + 1,nx) + nx*iy; 
	      zd = WRAP_AROUND(ix - 1,nx) + nx*iy; 

	      /* if odd-numbered row */
 	      if(iy&1) 
		{
		  zb = WRAP_AROUND(ix+1,nx) + nx*jb;
		  zf = WRAP_AROUND(ix+1,nx) + nx*jf;
 		  ze = ix + nx*je;
		  zc = ix + nx*jc;
		}
	      else
		{
		  zb = ix + nx*jb;
		  zf = ix + nx*jf;
		  ze = WRAP_AROUND(ix-1,nx) + nx*je;
		  zc = WRAP_AROUND(ix-1,nx) + nx*jc;
		}
	      conf = table[latt[iz] + eps];

/* Propagation:  mask and copy relevant bits */

	      newlatt[za] |= conf & A; 
	      newlatt[zb] |= conf & B; 
	      newlatt[zc] |= conf & C; 
	      newlatt[zd] |= conf & D; 
	      newlatt[ze] |= conf & E; 
	      newlatt[zf] |= conf & F; 
	      newlatt[iz] |= conf & S;
	    }
	}
      /* We exchange pointers (a trick that saves a few operations */

      ptr=latt;
      latt=newlatt;
      newlatt=ptr;
    

/* average momentum in both x, y direction and average them over TAVG */
      if(t>TMAX)
	{
	  for(iy=0;iy<ny; iy++)
	    {
	      for(ix = 0; ix< nx; ix++)
		{

		  iz = ix+iy*nx;
		  xmom[iz] += X_MOM(latt[iz]);
		  ymom[iz] += Y_MOM(latt[iz]);
		  mass[iz] += PARTSUM(latt[iz]);

		}
	    }
	}

      if(t%TPRINT == 0 ) 
	{
	  fprintf(stderr,"t=%d\n",t); 
	  test_cons(latt,&nz,&px,&py,&npart); 
	}

    }

 
        
/* write momentum to ascii file , where ix is slow variable and iy fast*/
  fprintf(stderr,"writing summed x,y,momentum to files...");
  for(ix=0;ix<nx; ix++)
    {
      for(iy = 0; iy< ny; iy++)
	{
	  iz = ix+iy*nx;
/* zero momentum in and at the walls */
	  if(!solid[iz])
	    {
	      fprintf(fpxmom,"%d ",xmom[iz]);
	      fprintf(fpymom,"%d ",ymom[iz]);

	    }
	  else
	    {
	      fprintf(fpxmom,"%d ",0);
	      fprintf(fpymom,"%d ",0);
	    }
	  fprintf(fpmass,"%d ",mass[iz]);
	}
      fprintf(fpxmom,"\n");
      fprintf(fpymom,"\n");
      fprintf(fpmass,"\n");
    }
  fprintf(stderr,"ok\n");
    
/* collect the garbage */
  free(latt);
  free(newlatt);
  free(solid);
  free(table);
  free(xmom);
  free(ymom);

  fprintf(stderr,"Normal exit from Fhp_simpl6 ! \n");
  exit(0); 
}


tabgen(table)
unsigned char *table;

{ 
   unsigned char conf, newconf;
   int i; 
  /* General case: no collision */  
  
  for(i=0;i<256 ;i++)
    {
      conf = i;
      /* Test if solid site */ 
      
      if((conf & S) == 0) 
	{
	  
	  /* It is not a solid site. Assume no collision */
	  
	  table[conf] = conf;
	}
      else
	{
	  
	  /* It is a solid site. */
	  /* Clear all bits except EPS and S */
	  



	  newconf = ( conf ) & (EPS | S);       
/* bounce back rule */
	  if(AP(conf)) newconf |= D;
	  if(BP(conf)) newconf |= E;
	  if(CP(conf)) newconf |= F;
	  if(DP(conf)) newconf |= A;
	  if(EP(conf)) newconf |= B;
	  if(FP(conf)) newconf |= C;

	  table[conf] = newconf;
	}
      newconf = table[conf];
/*      printf("conf %d  newconf %d \n", (int) conf, (int) newconf);
      printf( " %d %d %d %d %d %d \n", (int) AP(conf), (int) BP(conf), (int) CP(conf), (int) DP(conf), (int) EP(conf), (int) FP(conf));
      printf( " %d %d %d %d %d %d \n", (int) AP(newconf), (int) BP(newconf), (int) CP(newconf), (int) DP(newconf), (int) EP(newconf), (int) FP(newconf));
*/
    }
  
  /* Put collisions */
  
  table[A + D]        = B + E;
  table[B + E]        = C + F;
  table[C + F]        = A + D;
  table[A + D + EPS]  = C + F;
  table[B + E + EPS]  = A + D;
  table[C + F + EPS]  = B + E;
  
  table[A + C + E] = B + D + F ;
  table[B + D + F] = A + C + E;
  table[A + C + E + EPS] = B + D + F + EPS;
  table[B + D + F + EPS] = A + C + E + EPS;

  table[A + B + D + E ] = A + C + D + F;
  table[B + C + E + F ] = A + B + D + E;
  table[A + C + D + F ] = B + C + E + F;
  table[A + B + D + E + EPS] = B + C + E + F;
  table[B + C + E + F + EPS] = A + C + D + F;
  table[A + C + D + F + EPS] = A + B + D + E;
}


test_cons(latt,nz,px,py,n)
int *px,*py,*n,*nz;
unsigned char *latt;
 {
  int iz;
  *px= *py= *n = 0;
  for(iz=0;iz< *nz; iz++)
    {
      *px += X_MOM(latt[iz]);
      *py += Y_MOM(latt[iz]);
      *n  += PARTSUM(latt[iz]);
    }
  fprintf(stderr," number: %d, x_mom %d, y_mom %d  \n",*n,*px,*py);
/*  fprintf(stdout," number: %d, x_mom %d, y_mom %d  \n",*n,*px,*py);*/
}
   










