#include <stdio.h>
#include <math.h>
#include <string.h>
#include <malloc.h>
#include <mex.h>
#include "randomlib.h"
#include "matrixjpl.h"

// *****************************************************
// mex semip_gcc file (semip_gcc.c)

// *****************************************************

// routine to draw rho from the conditional posterior distribution
double draw_rho(double *rvec,
				double *ldet,
				double epe0,
				double eped,
				double epe0d,
				double sige,
				int nrho,
				int n,
				int k,
     			double rho)
{

	int i;
	double adj, nmk, nmk2, rnd;
	double *s, *den, *num;
	double dsum, rsum, z;
	

// compute denominator (integrating constant)
	      s    = dvector(0,nrho-1);
	      den  = dvector(0,nrho-1);
	      num  = dvector(0,nrho-1);

		  nmk = (double)(n-k);
		  nmk2 = nmk/2;	  

	  for(i=0; i<nrho; i++){
		      z = epe0 - 2.0*rvec[i]*eped + rvec[i]*rvec[i]*epe0d;
              s[i] = ldet[i] - (z/2*sige);
	  }

// adjustment for scaling that subtracts the maximum value
	adj = s[0];
	for(i=1; i<nrho; i++){
		if (s[i] > adj)
			adj = s[i];
	}
	
	for(i=0; i<nrho; i++){
		den[i] = (s[i] - adj);
		den[i] = exp(den[i]);
		}

	dsum = 0.0; // trapezoid rule
    for(i=0; i<nrho-1; i++){
		dsum = dsum + (rvec[i+1]+rvec[i])*(den[i+1]-den[i])/2;
	}

	    rsum = 0.0;
		for(i=0; i<nrho; i++){
		s[i] = dabs(den[i]/dsum); // normalize rho post
        rsum = rsum + s[i];
		if (i == 0){
		den[i] = s[i];
		} else {
		den[i] = den[i-1] + s[i]; // cumulative sum
		}
	}
	
    rnd = rsum*ranf();

	// create rho draw via inversion
	for(i=0; i<nrho; i++){
	  if (rnd <= den[i]){
	  rho = rvec[i];
	  break;
	  }
	  }
	  	  
	free_dvector(s,0);
	free_dvector(den,0);
	free_dvector(num,0);

	return (rho);

}




// *****************************************************
// MAIN SAMPLER
// *****************************************************

// Estimates robust spatial probit model using MCMC

// semip_gc(bdraw,adraw,pdraw,sdraw,rdraw,vmean,amean,zmean,yhat,cntr,
// y,x,W,ndraw,nomit,nsave,n,k,m,mobs,a,nu,d0,rval,mm,kk,detval,ngrid,TI,TIc)
     

void semip_gc(

		//Output Arguments

        double *bdraw, // draws for beta (ndraw - nomit,k) matrix
		double *adraw, // draws for regional effects, a, (m,1) vector
        double *pdraw, // draws for rho (ndraw - nomit,1) vector
		double *sdraw, // draws for sige (ndraw - nomit,1) vector
		double *rdraw, // draws for rval (ndraw - nomit,1) vector (if mm != 0)
		double *vmean, // mean of vi draws (n,1) vector			
        double *amean, // mean of a-draws (m,1) vector
        double *zmean, // mean of latent z-draws (n,1) vector
        double *yhat,  // mean of posterior predicted y (n,1) vector
            
		//Input Arguments

		double *y,		// (n,1) lhs vector with 0,1 values
 		double *x,		// (n,k) matrix of explanatory variables   
 		double *W,		// (m,m) weight matrix
        int ndraw,		// # of draws
        int nomit,		// # of burn-in draws to omit
		int nsave,		// # of draws saved (= ndraw - nomit)
        int n,			// # of observations
		int k,			// # of explanatory variables
        int m,			// # of regions
        int *mobs,		// (m,1) vector of obs numbers in each region
		double *a,		// (m,1) vector of regional effects
		double nu,		// prior parameter for sige	 
        double d0,		// prior parameter for sige
        double rval,	// hyperparameter r            
        double mm,		// prior parameter for rval
        double kk,		// prior parameter for rval
        double *detval, // (ngrid,2) matrix with [rho , log det values]            
        int ngrid,		// # of values in detval (rows)
        double *TI,		// prior var-cov for beta (inverted in matlab)
		double *TIc)	// prior var-cov * prior mean	

{
    
// Local Variables

	int i,j,l,iter,invt,accept,rcount,cobs,obsi,zflag;
    double rmin,rmax,sige,chi,ee,ratio,p,rho,rho_new;
	double junk,aBBa,vsqrt,ru,rhox,rhoy,phi,awi,aw2i,bi,di;
    double *z,*tvec,*e,*e0,*xb,*mn,*ys,*limit1,*limit2,*zmt;
	double *bhat,*b0,*b0tmp,*v,*vv,*b1,*b1tmp,*Bpa,*rdet,*ldet,*w1;
	double **xs,**xst,**xsxs,**A0,**A1,**Bpt,**Bp,**xmat,**W2; 	
	double *Wa, **Wmat;
	double c0, c1,c2;

		   
// Allocate Matrices and Vectors


	  xs     = dmatrix(0,n-1,0,k-1);	  
	  xst    = dmatrix(0,k-1,0,n-1);      
	  xsxs   = dmatrix(0,k-1,0,k-1);
	  A0     = dmatrix(0,k-1,0,k-1);	  
	  A1     = dmatrix(0,m-1,0,m-1);
	  Bp     = dmatrix(0,m-1,0,m-1);
	  Bpt    = dmatrix(0,m-1,0,m-1);
	  xmat	 = dmatrix(0,n-1,0,k-1);
	  W2     = dmatrix(0,m-1,0,m-1);
	  Wmat   = dmatrix(0,m-1,0,m-1);

	   z     = dvector(0,n-1);
	   tvec  = dvector(0,n-1);
	   e     = dvector(0,n-1);
	   e0    = dvector(0,n-1);
	   xb    = dvector(0,n-1);
	   mn    = dvector(0,n-1);
	   ys    = dvector(0,n-1);      
      limit1 = dvector(0,n-1);			
	  limit2 = dvector(0,n-1);
	  zmt	 = dvector(0,n-1);
	  vv	 = dvector(0,n-1);
	  bhat   = dvector(0,k-1);
	  b0     = dvector(0,k-1);
	  b0tmp  = dvector(0,k-1);
	  v      = dvector(0,m-1);
	  b1     = dvector(0,m-1);
	  b1tmp  = dvector(0,m-1);
	  Bpa    = dvector(0,m-1);
	  w1     = dvector(0,m-1);
	  rdet   = dvector(0,ngrid-1);
	  ldet   = dvector(0,ngrid-1);	 
	  Wa     = dvector(0,m-1);
   
// Initializations


    junk = 0.0;  // Placeholder for mean in meanvar()
  	rho = 0.5;  
	sige = 1.0;
	zflag = 0;  // a flag for 0,1 y-values

// Initialize (z,vv,limits,tvec) 

	for(i=0; i<n; i++){
        z[i] = y[i];
		vv[i] = 1.0;
		tvec[i] = 1.0;
	    if (y[i] == 0.0){
			limit1[i] = -10000;
			limit2[i] = 0.0;
			zflag = 1; // we have 0,1 y-values so sample z
	    }else{
			limit1[i] = 0.0;
			limit2[i] = 10000;
		}
	}
        
	
	// Initialize v, b1tmp, Bp


	for(i=0; i<m; i++){
		v[i] = 1.0;
	    b1tmp[i] = 1.0;		                 
        for(j=0; j<m; j++){
              if (j == i){
                 Bp[i][j] = 1 - rho*W[i + j*m];
			  }else{
                 Bp[i][j] = - rho*W[i + j*m];
              }
              Wmat[i][j] = W[i + j*m];
		}
	}


	
	// Parse detval into rdet and ldet vectors 
	//       and define (rmin,rmax)
	
	for(i=0; i<ngrid; i++){
	    j=0;
		rdet[i] = detval[i + j*ngrid];
	    j=1;
		ldet[i] = detval[i + j*ngrid];
	}
	
	rmin = rdet[0];
    rmax = rdet[ngrid-1];  

	
	// Put x into xmat

	for(i=0; i<n; i++){
		for(j=0; j<k; j++)
            xmat[i][j] = x[i + j*n];
	}


	// Compute matrices to be used for updating a-vector

	if(m > 100){  // Used only for large m

		for(i=0; i<m; i++){

			w1[i] = 0.0;							// Form w1[i]
			for(j=0;j<m;j++){
				w1[i] = w1[i] + (W[j + i*m]*W[j + i*m]);
			}

			for(j=0;j<m;j++){						// Form W2[i][*]
				W2[i][j] = 0.0;
				if(j != i){					
					for(l=0;l<m;l++){
						W2[i][j] = W2[i][j] + W[l + i*m]*W[l + j*m];
					}
				}		// Note that W2[i][i] = 0.0 by construction
			}			
		}
	} // End if(m > 10)



// ======================================
// Start the Sampler
// ======================================

	for(iter=0; iter<ndraw; iter++){


// UPDATE: beta

// (1) Apply stnd devs (vsqrt) to x, z, z - tvec
	
	for(i=0; i<n; i++){
		vsqrt = sqrt(vv[i]);
	    zmt[i] = (z[i] - tvec[i])/vsqrt;
		for(j=0; j<k; j++)
			xs[i][j] = x[i + j*n]/vsqrt;
	}
	      

//  (2) Construct A0 matrix


	transpose(xs,n,k,xst);				// form xs'
	  matmat(xst,k,n,xs,k,xsxs);		// form xs'*xs
      for(i=0; i<k; i++){				// form xs'*xs + TI
            for(j=0; j<k; j++)
                 A0[i][j] = xsxs[i][j] + TI[i + j*k];
          }      

      invt = inverse(A0, k);			// replace A0 = inv(A0)
	  if (invt != 1)
		 mexPrintf("semip_gc: Inversion error in beta conditional \n");

// (3) Construct b0 vector

	matvec(xst,k,n,zmt,b0tmp);			//form xs'*zmt
      for(i=0; i<k; i++){
		b0tmp[i] = b0tmp[i] + TIc[i];		//form b0tmp = xs'*zmt + TIc
      }
	matvec(A0,k,k,b0tmp,b0);				//form b0 = A0*b0tmp

// (4) Do multivariate normal draw from N(b0,A0)

	normal_rndc(A0, k, bhat);			// generate N(0,A0) vector
	for(i=0; i<k; i++){
		bhat[i] = bhat[i] + b0[i];		// add mean vector, b0
	}

// (5) Now update related values:


	matvec(xmat,n,k,bhat,xb);			// form xb = xmat*bhat


	for(i=0; i<n; i++){					// form e0 = z - x*bhat
		e0[i] = z[i] - xb[i];	
	}

	cobs = 0;
	for(i=0; i<m; i++){					// form b1tmp = e0i/v[i]
		obsi = mobs[i];
		b1tmp[i] = 0.0;
		for(j=cobs; j<cobs + obsi; j++){
			b1tmp[i] = b1tmp[i] + (e0[j]/v[i]);	
		}
		cobs = cobs + obsi;
	}
	
        

// UPDATE: a 
	
	if(m <= 100){  // For small m use straight inverse

		// (1) Define A1 and b1

		transpose(Bp,m,m,Bpt);			    // form Bp'
		matmat(Bpt,m,m,Bp,m,A1);			// form Bp'*Bp
	
		for(i=0; i<m; i++){					// form A1 = (1/sige)*Bp'Bp + diag(mobs/v)
			for(j=0; j<m; j++){
				if (j == i){
					A1[i][j] = (1/sige)*A1[i][j] + ((double)mobs[i])/v[i];
				}else{
					A1[i][j] = (1/sige)*A1[i][j];
				}
			}
		}
	

		inverse(A1,m);						// set A1 = inv(A1)
	
		matvec(A1,m,m,b1tmp,b1);			// form b1

	
		// (2) Do multivariate normal draw from N(b1,A1)

		normal_rndc(A1,m,a);				// generate N(0,A1) vector
		for(i=0; i<m; i++){
			a[i] = a[i] + b1[i];			// add mean vector, b1
		}

	}else{   // For large m use marginal distributions

		cobs = 0;
		for(i=0;i<m;i++){			
			obsi = mobs[i];
			
			phi = 0.0;
			for(j=cobs;j<cobs+obsi;j++){
				phi = phi + ((z[j] - xb[j])/v[i]);  // form phi
			}		
			awi = 0.0;
			aw2i = 0.0;
			for(j=0;j<m;j++){						// form awi and aw2i
				aw2i = aw2i + W2[i][j]*a[j];		// (W2[i][i] = 0)	
				if(j != i){
					awi = awi + (W[i + j*m] + W[j + i*m])*a[j];
				}
			}
			bi = phi + (rho/sige)*awi - ((rho*rho)/sige)*aw2i;
			di = (1/sige) + ((rho*rho)/sige)*w1[i] + (obsi/v[i]);		
			a[i] = (bi/di) + sqrt(1/di)*snorm();	// Form a[i]           

			cobs = cobs + obsi;
		}
	} // End update of a
	


	// Compute tvec = del*a	
	cobs = 0;
	for(i=0; i<m; i++){
		obsi = mobs[i];
		for(j=cobs; j<cobs + obsi; j++){
			tvec[j] = a[i];
		}
		cobs = cobs + obsi;
	}

//UPDATE: sige

	matvec(Bp,m,m,a,Bpa);			//form Bp*a
	aBBa = 0.0;						//form aBBa = a'*Bp'*Bp*a
	for(i=0; i<m; i++){		
		aBBa = aBBa + Bpa[i]*Bpa[i];
	}
	
	chi = genchi(m + 2.0*nu);

	sige = (aBBa + 2.0*d0)/chi;

	
//UPDATE: v (and vv = del*v)	
	

	for(i=0; i<n; i++){
		e[i] = e0[i] - tvec[i];		//form e  = z - x*bhat - tvec
	}
	cobs = 0;
	for(i=0; i<m; i++){
		obsi = mobs[i];
		chi = genchi(rval + obsi);
		ee = 0.0;
		for(j=cobs; j<cobs + obsi; j++){		// form ee
			ee = ee + e[j]*e[j];
		}
		v[i] = (ee + rval)/chi;					// form v
		for(j=cobs; j<cobs + obsi; j++){
			vv[j] = v[i];						// form vv
		}
		cobs = cobs + obsi;
	}
	


//UPDATE: rval (if necessary)

	if (mm != 0.0){
    rval = gengam(mm,kk);
    }

//UPDATE: rho (using univariate integration)

	      matvec(Wmat,m,m,a,Wa); // form Wa vector

		  	c0 = 0.0;						// form a'*a
		  	c1 = 0.0;                       // form a'*Wa
		  	c2 = 0.0;                       // form (Wa)'*Wa
	        for(i=0; i<m; i++){		
		    c0 = c0 + a[i]*a[i];
		    c1 = c1 + a[i]*Wa[i];
		    c2 = c2 + Wa[i]*Wa[i];
	        }

	        rho = draw_rho(rdet,ldet,c0,c1,c2,sige,ngrid,m,k,rho);



// (4) Update Bp matrix using new rho

	 
	for(i=0; i<m; i++){		                 
        for(j=0; j<m; j++){
              if (j == i){
                 Bp[i][j] = 1 - rho*W[i + j*m];
			  }else{
                 Bp[i][j] = - rho*W[i + j*m];
              }
		}
	}

	 
//UPDATE: z 
if (zflag == 1){ // skip this for continuous y-values
	 // (1) Generate vector of means

	 cobs = 0;
	 for(i=0; i<m; i++){
		 obsi = mobs[i];
		 for(j=cobs; j<cobs + obsi; j++){
			 mn[j] = xb[j] + a[i];
		 }			 		 
		 cobs = cobs + obsi;		 
	 }
	 

	 // (2) Sample truncated normal for latent z values

	 normal_truncc(n,mn,vv,limit1,limit2,z);

	 // (3) Compute associated sample y vector: ys

	 for(i=0; i<n; i++){
		 if(z[i]<0){
			 ys[i] = 0.0;
		 }else{
			 ys[i] = 1.0;
		 }
	 }

} // end of if zflag == 1


// ===================
// Save sample draws 	
// ===================


	 
	 if (iter > nomit-1){
		 pdraw[iter - nomit] = rho;							//save rho-draws
		 sdraw[iter - nomit] = sige;					    //save sige-draws	 
		 if(mm != 0.0)
			rdraw[iter - nomit] = rval;						//save r-draws (if necessary)
	     for(j=0; j<k; j++)
			bdraw[(iter - nomit) + j*nsave] = bhat[j];		//save beta-draws
		 for(i=0; i<m; i++){
			vmean[i] = vmean[i] + v[i]/((double) (nsave));	//save mean v-draws
			adraw[(iter - nomit) + i*nsave] = a[i];			//save a-draws
			amean[i] = amean[i] + a[i]/((double) (nsave));	//save mean a-draws
		 }
	     for(i=0; i<n; i++){
			zmean[i] = zmean[i] + z[i]/((double) (nsave));	//save mean z-draws
			yhat[i] = yhat[i] + mn[i]/((double) (nsave));   //save mean y-values
		 }
	 }

	 } // End iteration loop
	 

// ===============================
// END SAMPLER
// ===============================


// Free up allocated vectors

	free_dmatrix(xs,0,n-1,0);	  	
	free_dmatrix(xst,0,k-1,0);       
	free_dmatrix(xsxs,0,k-1,0);
	free_dmatrix(A0,0,k-1,0);
	free_dmatrix(A1,0,m-1,0);  
	free_dmatrix(Bp,0,m-1,0);  
    free_dmatrix(Bpt,0,m-1,0);
	free_dmatrix(W2,0,m-1,0);
	free_dmatrix(xmat,0,n-1,0);
	free_dmatrix(Wmat,0,m-1,0);


	free_dvector(z,0);
	free_dvector(tvec,0);   
	free_dvector(e,0);   
	free_dvector(e0,0);
	free_dvector(xb,0);
	free_dvector(mn,0);
	free_dvector(ys,0);      
    free_dvector(limit1,0);			
	free_dvector(limit2,0);
	free_dvector(zmt,0);
	free_dvector(vv,0);
	free_dvector(bhat,0);
	free_dvector(b0,0);
	free_dvector(b0tmp,0);
	free_dvector(v,0);
	free_dvector(b1,0);
	free_dvector(b1tmp,0);
	free_dvector(Bpa,0);
	free_dvector(rdet,0);
	free_dvector(ldet,0);
	free_dvector(w1,0);
	free_dvector(Wa,0);


} // End of semip_gc



//************************

// START GATEWAY FUNCTION

//************************

void mexFunction( int nlhs, mxArray *plhs[],
                  int nrhs, const mxArray *prhs[] )
{
  
// Local variables for the gateway function

// semip_gc(bdraw,adraw,pdraw,sdraw,rdraw,vmean,amean,zmean,yhat,
// y,x,W,ndraw,nomit,nsave,n,k,m,mobs,a,nu,d0,rval,mm,kk,detval,ngrid,TI,TIc)
     
	// (The inputs are passed to the subroutine and the outputs
	//  are extracted from the subroutine and passed back to Matlab.)

  double *bdraw,*adraw,*pdraw,*sdraw,*rdraw,*vmean,*amean,*zmean,*yhat;
  double *y,*x,*W,*a,nu,d0,rval,mm,kk,*detval,ngrid,*TI,*TIc,*m_obs;
  int i,ndraw,nomit,nsave,n,k,m,*mobs;
  
  long *Seed1, *Seed2;
  int buflen, status, flag;
  char *phrase;
  static char phrase2[8];
  
    phrase2[0] = 'h';
    phrase2[1] = 'h';
    phrase2[2] = 'i';
    phrase2[3] = 't';
    phrase2[4] = 'h';
    phrase2[5] = 'e';
    phrase2[6] = 'r';
    phrase2[7] = 'e';



  /* Check for proper number of arguments. */

  if(nrhs == 20) {  
     flag = 0;
    phrase = phrase2;
  } else if (nrhs == 21){
    flag = 1;
  } else {
    mexErrMsgTxt("semip_gc: 20 or 21 inputs required.");
  }

 
  if(nlhs != 9) {
    mexErrMsgTxt("semip_gcc: 9 output arguments needed");
  }


    if (flag == 1) {

    // input must be a string
    if ( mxIsChar(prhs[20]) != 1)
      mexErrMsgTxt("semip_gc: seed must be a string.");
    // input must be a row vector
    if (mxGetM(prhs[20])!=1)
      mexErrMsgTxt("semip_gc: seed input must be a row vector.");

    // get the length of the input string
    buflen = (mxGetM(prhs[20]) * mxGetN(prhs[20])) + 1;

    // allocate memory for input string
    phrase = mxCalloc(buflen, sizeof(char));

    // copy the string data from prhs[0] into a C string input_ buf.
    // If the string array contains several rows, they are copied,
    // one column at a time, into one long string array.
    //
    status = mxGetString(prhs[20], phrase, buflen);
    if(status != 0)
      mexWarnMsgTxt("semip_gc: Not enough space. seed string truncated.");
    }
    
    // allow the user to set a seed or rely on clock-based seed
   if (flag == 0) {
    setSeedTimeCore(phrase);
   } else {
	phrtsd(phrase,&Seed1,&Seed2);
    setall(Seed1,Seed2);
   }


//  Parse input arguments

// semip_gcc(y,x,W,ndraw,nomit,nsave,n,k,m,mobs,a,nu,d0,rval,mm,kk,detval,ngrid,TI,TIc);

     y = mxGetPr(prhs[0]);
     x = mxGetPr(prhs[1]);
	 W = mxGetPr(prhs[2]);
	 ndraw = (int) mxGetScalar(prhs[3]);
	 nomit = (int) mxGetScalar(prhs[4]);
	 nsave = (int) mxGetScalar(prhs[5]);
	 n = (int) mxGetScalar(prhs[6]);
	 k = (int) mxGetScalar(prhs[7]);
	 m = (int) mxGetScalar(prhs[8]);
	 m_obs = mxGetPr(prhs[9]);
     a = mxGetPr(prhs[10]);
	 nu = mxGetScalar(prhs[11]);
	 d0 = mxGetScalar(prhs[12]);
	 rval = mxGetScalar(prhs[13]);
	 mm = mxGetScalar(prhs[14]);
	 kk = mxGetScalar(prhs[15]);
	 detval = mxGetPr(prhs[16]);
	 ngrid = (int) mxGetScalar(prhs[17]);	 
	 TI = mxGetPr(prhs[18]);
	 TIc = mxGetPr(prhs[19]);
	 

	// no need for error checking on inputs
	// since this was done in the matlab function

	  // Transfer m_obs into an integer vector

	 //(1) Make integer vector storage
	 
	 mobs = ivector(0,m-1);

	 //(2) Put m_obs into mobs

	for(i=0; i<m; i++){
		mobs[i] = (int) m_obs[i];
	}
			

    /* Create matrix mxArrays for the return arguments */
	 //[bdraw,adraw,pdraw,sdraw,rdraw,vmean,amean,zmean,yhat,cntr]

    plhs[0] = mxCreateDoubleMatrix(nsave,k, mxREAL);	// beta draws
	plhs[1] = mxCreateDoubleMatrix(nsave,m, mxREAL);	// a-draws
	plhs[2] = mxCreateDoubleMatrix(nsave,1, mxREAL);	// rho draws
	plhs[3] = mxCreateDoubleMatrix(nsave,1, mxREAL);	// sige draws
	plhs[4] = mxCreateDoubleMatrix(nsave,1, mxREAL);	// r-draws
	plhs[5] = mxCreateDoubleMatrix(m,1, mxREAL);		// v-means
	plhs[6] = mxCreateDoubleMatrix(m,1, mxREAL);		// a-means
	plhs[7] = mxCreateDoubleMatrix(n,1, mxREAL);		// z-means
	plhs[8] = mxCreateDoubleMatrix(n,1, mxREAL);		// yhat-means


	// Get pointers to the data in the mxArrays for the C-subroutine


    bdraw = mxGetPr(plhs[0]);
	adraw = mxGetPr(plhs[1]);
	pdraw = mxGetPr(plhs[2]);	
	sdraw = mxGetPr(plhs[3]);
	rdraw = mxGetPr(plhs[4]);
	vmean = mxGetPr(plhs[5]);
	amean = mxGetPr(plhs[6]);
	zmean = mxGetPr(plhs[7]);
	yhat  = mxGetPr(plhs[8]);

// Call the Subroutine

    semip_gc(bdraw,adraw,pdraw,sdraw,rdraw,vmean,amean,zmean,yhat,y,x,W,ndraw,nomit,nsave,n,k,m,mobs,a,nu,d0,rval,mm,kk,detval,ngrid,TI,TIc);

// free up mobs
	
	free_ivector(mobs,0);


}


