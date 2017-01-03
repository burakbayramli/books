/* 
   FINDS CLUSTERS/(CLIQUES) IN A GRAPH.

   call with :
   ./cliquedecomp LinkFile OutputFile InitialFile ClusterNumber Zloops Aloops beta Abeta Bbeta Vtoupdate DisplayError BurninSteps BurninBeta
   
   For example:

   ./cliquedecomp polbooks_links.txt CliqueMatrix.txt random 200 20 10 10 1 20 25 1 5 0.01

   LinkFile : a txt file containing on each line an element of the adjacency matrix, e.g.  3 5

   OutputFile :  the file on which the (transposed) clique matrix Z is stored
   
   InitialFile : File containin initial clique matrix (in transposed order -- ie C rows, V cols). Only the top ClusterNumber rows are used If InitFile=random, 
   then a random initialisation is used, containing ClusterNumber clusters

   ClusterNumber : initial number of clusters to find. The algorithm attempts to find a smaller number.
   If 0, the uses the number of clusters in InitialFile.

   Zloops : number of internal loops for fixed number of clusters

   Aloops : number of exterior loops to try to find smallest number of clusters

   beta : steepness of the logistic function (typically 10, but make higher to force low errors, make lower to accept errors)

   Abeta : Beta function parameter (typically 1)

   Bbeta : Beta function parameter (typically 10 -- make this higher if you want less clusters, but more errors)

   Vtoupdate : the number of vertices to updates during the z-loop of the mean-field. Normally one would set this to the number of vertices in the graph, but setting it lower gives a faster approximate implementation. If set to zero, all vertices are updated.

   DisplayError: set to 1 to display the errors (expensive to compute), otherwise set to 0

   Burn in : Use a burn in of BurninSteps with BurninBeta 

   The adjacency matrix is approximated by A=ZZ' (under threshold arithmetic)
 

   David Barber : University College London, October 2007
   ------------------------------------------------------

   Compile with

   gcc -O3 cliqedecomp.c -o cliquedecomp

   ---------------------------------

   Works under :

   Windows XP (Cygwin)

*/ 

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
//#include <math.h>


#define MAXV 250
#define MAXC 3000
#define MAXLINKS 100000
//#define DISP_ERRORS 1 /* set this to 0 to speed up by not displaying errors */
#define DISP_A 0
#define SMALLVALUE 0.1
#define THRESHOLD 0.5

int max_value(int p_array[MAXLINKS], int numlinks)
{
  int position, p_max_value;
  
  position = 0;
  p_max_value = p_array[position];
  for (position = 1; position < numlinks; ++position)
    {
      if (p_array[position] > p_max_value)
	{p_max_value = p_array[position];}
    }
  return p_max_value;
}

int* randperm(int N)
{
  int*ord  = malloc(N*sizeof(int));
  int i=0;
  for (i = 0; i < N; ++i) {
    int j = rand() % (i + 1);
    ord[i] = ord[j];
    ord[j] = i;
  }
  return ord;
}


float gammaln(float xx)
{
  static float x,y,tmp,ser;
  static float cof[6]={76.18009172947146,-86.50532032941677,
		       24.01409824083091,-1.231739572450155,
		       0.1208650973866179e-2,-0.5395239384953e-5};
  int j;
  y=x=xx;
  tmp=x+5.5;
  tmp -= (x+0.5)*log(tmp);
  ser=1.000000000190015;
  for (j=0;j<=5;j++) ser += cof[j]/++y;
  return -tmp+log(2.5066282746310005*ser/x);
}

float betalog(float x, float y)
{return gammaln(x)+gammaln(y)-gammaln(x+y);}


float logsigma(float x, float beta, float Threshold)
{
  static float y;
  y = beta*(Threshold-x);
  if (y<0.0){return -log(1.0+exp(y));}
  else {return -y-log(1.0+exp(-y));};
}

    
float clogsigma(float x, float beta, float Threshold)
{
  static float y;
  y = beta*(Threshold-x);
  if (y<0.0){return y-log(1.0+exp(y));}
  else {return -log(1+exp(-y));}
}


int aintprint(bool a[MAXV][MAXV], int l)
{     int i,j;
  for (i=0;i<l;i++){
    for (j=0;j<l;j++){
      printf("%d ",a[i][j]);
    }
    printf("\n");
  }
  return;}


int zfloatprint(float z[MAXV][MAXC], int V, int C)
{   static  int i,j;
  for (i=0;i<C;i++){
    for (j=0;j<V;j++){
      printf("%3.2f ",z[i][j]);
    }
    printf("\n");
  }
  printf("\n");
  return;}

int mafloatprint(float ma[MAXC], int C)
{     static int i;
  for (i=0;i<C;i++){
    printf("%3.2f ",ma[i]);
  }
  printf("\n");
  
  return;}

float loglik(float z[MAXV][MAXC], bool A[MAXV][MAXV],int V, int C, float beta, float Threshold)
{
  static float loglik, field;
  static int i,j,c;
  loglik=0.0;
  for (i=0;i<V;i++){
    for (j=0;j<i+1;j++){ // check if can do this -- otherwise use j<V
      field=0.0;
      for (c=0;c<C;c++){
	field+=z[i][c]*z[j][c];
      }
      if (A[i][j])
	{loglik+=logsigma(field,beta,Threshold);}
      else
	{loglik+=clogsigma(field,beta,Threshold);}
    }
  }
  return loglik;
}

void updateZ(char OutputFile[40], float zm[MAXV][MAXC], float zma[MAXV][MAXC], float ma[MAXC], bool A[MAXV][MAXV], int Zloops, int C, int V, int Vtoupdate, float beta, float Threshold,bool DISP_ERRORS) 
{int *permC, *permV, loopz, loopc, cluster, loopvertexj, loopvertexk, vertexi,vertexk, vertexj, cl, i, j,k, errors, tmpi, tmpj;
  float meanfield0,meanfield1,logfn1,logfn0,loglik1,loglik0,tmp, copybeta, dellog;
  float Mold[MAXV][MAXV], zc[MAXV], Mk[MAXV];
  static bool AA[MAXV][MAXV];
  FILE *fpLinkFile, *fpOutputFile, *fpInitialFile;

  for (loopz=0;loopz<Zloops;loopz++)
    {
      for (vertexi=0;vertexi<V;vertexi++)
	{for (vertexj=0;vertexj<V;vertexj++)
	    {
	      Mold[vertexi][vertexj]=0.0;
	      for (cl=0;cl<C;cl++){
		Mold[vertexi][vertexj]+=zma[vertexi][cl]*zma[vertexj][cl];}
	    }
	}
      
      permC=randperm(C);	      
      for (loopc=0;loopc<C;loopc++)
	{cluster=permC[loopc];
	  
	  for (vertexi=0;vertexi<V;vertexi++) {zc[vertexi]=zma[vertexi][cluster];}
	  permV=randperm(V);
	  for (loopvertexk=0;loopvertexk<Vtoupdate;loopvertexk++)
	    {vertexk=permV[loopvertexk];
	      for (vertexi=0;vertexi<V;vertexi++)
		{Mk[vertexi]=Mold[vertexi][vertexk]-zc[vertexi]*zc[vertexk]+zma[vertexi][cluster]*zma[vertexk][cluster];}
	      logfn1=0.0;logfn0=0.0;
	      for (vertexj=0;vertexj<V;vertexj++)
		{meanfield0=Mk[vertexj]-zma[vertexj][cluster]*zma[vertexk][cluster]-0.5;
		  meanfield1=zma[vertexj][cluster]+meanfield0; 
		  if (A[vertexj][vertexk]==1)
		    {logfn1+=logsigma(meanfield1,beta,0.0);
		      logfn0+= logsigma(meanfield0,beta,0.0);
		    }
		  else
		    {logfn1+= logsigma(-meanfield1,beta,0.0);
		      logfn0+=  logsigma(-meanfield0,beta,0.0);
		    }
		}
	      dellog=2*(logfn0-logfn1);
	      zm[vertexk][cluster]=1.0/(1.0+exp(dellog));
	      zma[vertexk][cluster]=zm[vertexk][cluster]*ma[cluster];
	    }
	  
	  for (vertexi=0;vertexi<V;vertexi++)
	    {for (vertexj=0;vertexj<V;vertexj++)
		{Mold[vertexi][vertexj]+=-zc[vertexi]*zc[vertexj]+zma[vertexi][cluster]*zma[vertexj][cluster];
		}
	    }
	}

      printf("\n C[%d] Zloop[%d] beta[%4.2f]",C,loopz,beta);
      /* compute the errors */	 
      if (DISP_ERRORS==1){
	errors=0;
	for (i=0;i<V;i++)
	  {
	    for (j=0;j<i+1;j++)
	      {tmp=0;
		for (k=0;k<C;k++)
		  {tmp+=zma[i][k]*zma[j][k];}
		if (tmp>Threshold){AA[i][j]=1;}
		else {AA[i][j]=0;}
		AA[j][i]=AA[i][j];
		if (AA[i][j]!=A[i][j]){errors+=+1;} 
	      }
	  }
	printf(", errors(threshold<Z><Z'>)=%d",errors);}
      
      
      /* compute the errors */	 
      if (DISP_ERRORS==1){
	errors=0;
	for (i=0;i<V;i++)
	  {
	    for (j=0;j<i+1;j++)
	      {tmp=0;
		for (k=0;k<C;k++)
		  {tmp+= (zm[i][k]>Threshold)*(zm[j][k]>Threshold);}
		if (tmp>0){AA[i][j]=1;}
		else {AA[i][j]=0;}
		AA[j][i]=AA[i][j];
		if (AA[i][j]!=A[i][j]){errors+=+1;} 
	      }
	  }
	printf(", errors(threshold<Z>threshold<Z'>)=%d",errors);}
      

      /* write out the cluster matrix (in transposed form): only those clusters that are on:*/
      fpOutputFile = fopen(OutputFile, "w");
      if(fpOutputFile==NULL){
	printf("Error: can't open OutputFile %s\n",OutputFile);}	  
      for (cluster=0;cluster<C;cluster++){
	if (ma[cluster]>SMALLVALUE){	 
	  for (i=0;i<V;i++){
	    fprintf(fpOutputFile, "%f ", zm[i][cluster]);}}
	fprintf(fpOutputFile,"\n");}
      fclose(fpOutputFile);
      //printf(" written (in transposed form) on %s\n",OutputFile);
    }
}

main(int argc, char *argv[])
{int *permC, *permV, *permV2;
  int loopz, loopa, loopc, cluster,loopvertexk,loopvertexj,j,i,k,Current,start,outloopc, newcluster;
  int V,vertexk,vertexj,cl,errors,numlinks;
  float meanfield0,meanfield1,logfn1,logfn0,loglik1,loglik0;
  float tmp;
  
  static float zma[MAXV][MAXC],zm[MAXV][MAXC], zmat[MAXV][MAXC];
  static float zmatmp[MAXV][MAXC],zmtmp[MAXV][MAXC], matmp[MAXC];
  static float ma[MAXC], atmp[MAXC], manew[MAXC], floatval[MAXV*MAXC];
  int c,cc;
  static bool A[MAXV][MAXV], AA[MAXV][MAXV]; 
  float T1, T0, logprior0, logprior1, logpost0, logpost1;
 
  char LinksInputFile[40], tmps[4];
  int C=atoi(argv[4]);
  int Zloops=atoi(argv[5]);
  int Aloops=atoi(argv[6]);
  float beta=atof(argv[7]);
  float Abeta=atof(argv[8]);
  float Bbeta=atof(argv[9]);
  //  float Threshold=atof(argv[10]);
  float Threshold=THRESHOLD;
  int Vtoupdate=atoi(argv[10]);
  bool DISP_ERRORS=atoi(argv[11]);
  int BurninSteps=atoi(argv[12]);
  float BurninBeta=atof(argv[13]);
  //  char OutputFile=argv[2];
  /* get the adjacency matrixc */
 
  FILE *fpLinkFile, *fpOutputFile, *fpInitialFile;
  int counter, vertex;
  int ii[MAXLINKS],jj[MAXLINKS];
  float fl;

  srand(time(0));

  bool dontstart=0;
  bool randominit=1;
  bool singleinit=0;
  
  fpLinkFile = fopen(argv[1], "r");
  if(fpLinkFile==NULL){
    printf("Error: can't open LinkFile %s\n",argv[1]);
    dontstart=1;}

  if (strcmp(argv[3],"random")==0)
    {singleinit=0; randominit=1;printf("\nRandom initialisation.\n");}

  if (strcmp(argv[3],"single")==0)
    {singleinit=1; randominit=1;printf("\nReplicated single cluster initialisation.\n");}
  
  if (randominit==0)
    {fpInitialFile = fopen(argv[3], "r");
      if(fpInitialFile==NULL){
	printf("Error: can't open InitialFile %s\n",argv[3]);
	dontstart=1; 
      }}
  
  if (dontstart==1){return 1;}
  
  else
    /* start the analysis : */  
    {
      counter=0;
      while(!feof(fpLinkFile)){ 
	fscanf(fpLinkFile, "%d %d", &ii[counter], &jj[counter]);
	counter++; 
      }      
      fclose(fpLinkFile);
      numlinks=counter; 
      printf("LinkFile read successfully.\n");
           
      /* find the number of vertices : */ 
      int nums[2];
      nums[0]=max_value(ii,numlinks);
      nums[1]=max_value(jj,numlinks);
      V = max_value(nums,2);
      printf("Number of vertices =%d\n",V);          
      if (Vtoupdate==0){Vtoupdate=V;}
      printf("Will update at each iteration %d randomly selected vertices\n",Vtoupdate);          

      if (randominit==0){
	cluster=0;
	while(!feof(fpInitialFile)){
	  for (vertex=0;vertex<V;vertex++){
	    fscanf(fpInitialFile, "%f ",&zm[vertex][cluster]);
	  }
	  ++cluster;
	}       
	fclose(fpInitialFile);
	printf("InitFile read successfully.\n");
	if (C==0){C=cluster;}}     
      
      /* setup mean alpha */
      for (i=0;i<C;i++){ma[i]=1.0;}
      
      /*   initialise Z   */
      for (i=0;i<C;i++)
	{ for (j=0;j<V;j++)
	    {if (randominit==1){zm[j][i]=1.0*(rand()>0.5*RAND_MAX);} /* initial to 0.05 not important */
	      zma[j][i]=ma[i]*zm[j][i]; 
	    }
	}
      /* setup adj matrix based on links */
      for (i=0;i<V;i++)
	{ for (j=0;j<i;j++)
	    {
	      A[j][i]=0; A[i][j]=0;
	    }
	  A[i][i]=1; 
	}
      for (counter=0;counter<numlinks;counter++)
	{A[ii[counter]-1][jj[counter]-1]=1;A[jj[counter]-1][ii[counter]-1]=1;}     
      
      if (singleinit==1)
	{printf("\nFinding first a single cluster based on random initialisation....\n");
	  updateZ(argv[2],zm,zma,ma,A,BurninSteps,1,V,V,BurninBeta,Threshold,DISP_ERRORS); 
	  for (i=1;i<C;i++)
	    { for (j=0;j<V;j++)
		{zm[j][i]=zm[k][0];zma[j][i]=ma[i]*zm[j][i]; 
		}
	    }
	}
      
	printf("\n\nFinding the clique matrix marginals p(z[vertex][cluster]=1|adjacency matrix):\n");

	if (BurninSteps>0){updateZ(argv[2],zm,zma,ma,A,BurninSteps,C,V,Vtoupdate,BurninBeta,Threshold,DISP_ERRORS);}
	
	for (loopa=0;loopa<=Aloops;loopa++)
	  {if (loopa==0) {updateZ(argv[2],zm,zma,ma,A,Zloops,C,V,Vtoupdate,beta,Threshold,DISP_ERRORS);}
	    else
	      {updateZ(argv[2],zm,zma,ma,A,Zloops,C,V,Vtoupdate,beta,Threshold,DISP_ERRORS);}
	    
	 if (loopa<Aloops){
	  /* update alpha */
	  printf("\n[%d]Updating number of clusters",loopa);
	  for (i=0;i<V;i++){
	    for (cluster=0;cluster<C;cluster++){zmat[i][cluster]=zma[i][cluster];}}
	  for (c=0;c<C;c++){manew[c]=0.0;}
	  
	  for (cluster=0;cluster<C;cluster++)
	    {
	      for (i=0;i<V;i++){zmat[i][cluster]=zm[i][cluster];}
	      loglik1=loglik(zmat,A,V,C,beta,Threshold);	 
	      for (i=0;i<V;i++){zmat[i][cluster]=0.0;}
	      loglik0=loglik(zmat,A,V,C,beta,Threshold);
	      for (i=0;i<V;i++){zmat[i][cluster]=zma[i][cluster];}
	      for (c=0;c<C;c++){atmp[c]=ma[c];}
	      atmp[cluster]=1.0;
	      T1=0.0;
	      for (c=0;c<C;c++){T1+=atmp[c];}
	      T0=1.0*C-T1;
	      logprior1=betalog(Abeta+T1,Bbeta+T0);
	      atmp[cluster]=0.0;
	      T1=0.0;
	      for (c=0;c<C;c++){T1+=atmp[c];}
	      T0=1.0*C-T1;
	      logprior0=betalog(Abeta+T1,Bbeta+T0);
	      logpost1=logprior1+loglik1;
	      logpost0=logprior0+loglik0;
	      ma[cluster]=1.0/(1.0+exp(logpost0-logpost1));
	    }	 	 
	  
	  cluster=0; newcluster=0; 
	  for (cluster=0;cluster<C;cluster++){
	    matmp[cluster]=ma[cluster];	    
	    for (i=0;i<V;i++){	  
	      zmatmp[i][cluster]=zma[i][cluster];
	      zmtmp[i][cluster]=zm[i][cluster];
	    }
	  }
	  for (cluster=0;cluster<C;cluster++){
	    if (ma[cluster]>SMALLVALUE){	 
	      for (i=0;i<V;i++){
		zma[i][newcluster]=zmatmp[i][cluster];
		zm[i][newcluster]=zmtmp[i][cluster];
	      }
		ma[newcluster]=matmp[cluster];
		newcluster++;		
	    }
	  }
	  C=newcluster;
	}
	}
    }
}
