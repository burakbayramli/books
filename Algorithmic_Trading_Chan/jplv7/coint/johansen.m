function result = johansen(x,p,k)
% PURPOSE: perform Johansen cointegration tests
% -------------------------------------------------------
% USAGE: result = johansen(x,p,k)
% where:      x = input matrix of time-series in levels, (nobs x m)
%             p = order of time polynomial in the null-hypothesis
%                 p = -1, no deterministic part
%                 p =  0, for constant term
%                 p =  1, for constant plus time-trend
%                 p >  1, for higher order polynomial
%             k = number of lagged difference terms used when
%                 computing the estimator
% -------------------------------------------------------
% RETURNS: a results structure:
%          result.eig  = eigenvalues  (m x 1)
%          result.evec = eigenvectors (m x m), where first
%                        r columns are normalized coint vectors
%          result.lr1  = likelihood ratio trace statistic for r=0 to m-1
%                        (m x 1) vector
%          result.lr2  = maximum eigenvalue statistic for r=0 to m-1 
%                        (m x 1) vector
%          result.cvt  = critical values for trace statistic
%                        (m x 3) vector [90% 95% 99%]
%          result.cvm  = critical values for max eigen value statistic
%                        (m x 3) vector [90% 95% 99%]                            
%          result.ind  = index of co-integrating variables ordered by
%                        size of the eigenvalues from large to small                                       
% -------------------------------------------------------
% NOTE: c_sja(), c_sjt() provide critical values generated using
%       a method of MacKinnon (1994, 1996).
%       critical values are available for n<=12 and -1 <= p <= 1,
%       zeros are returned for other cases.
% -------------------------------------------------------
% SEE ALSO: prt_coint, a function that prints results
% -------------------------------------------------------
% References: Johansen (1988), 'Statistical Analysis of Co-integration
% vectors', Journal of Economic Dynamics and Control, 12, pp. 231-254.
% MacKinnon, Haug, Michelis (1996) 'Numerical distribution 
% functions of likelihood ratio tests for cointegration', 
% Queen's University Institute for Economic Research Discussion paper.
% (see also: MacKinnon's JBES 1994 article 
% -------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

% ****************************************************************
% NOTE: Adina Enache provided some bug fixes and corrections that
%       she notes below in comments. 4/10/2000
% ****************************************************************

% error checking on inputs
if (nargin ~= 3)
 error('Wrong # of inputs to johansen');
end;
[nobs m] = size(x);

     if (p > -1);
        f = 0;
     else ;
        f = p;
     end;

     x     = detrend(x,p);
     dx    = tdiff(x,1);
     dx    = trimr(dx,1,0);
     z     = mlag(dx,k);
     z     = detrend(trimr(z,k,0),f);
     dx    = detrend(trimr(dx,k,0),f);     
     r0t   = dx - z*(z\dx);
     dx    = detrend(trimr(lag(x,k),k+1,0),f);
     rkt   = dx - z*(z\dx);     
     skk   = rkt'*rkt/rows(rkt);
     sk0   = rkt'*r0t/rows(rkt); 
     s00   = r0t'*r0t/rows(r0t); 
     sig   = sk0*inv(s00)*(sk0');
     tmp   = inv(skk); 
     [du au] = eig(tmp*sig);
     orig = tmp*sig;

% Normalize the eigen vectors such that (du'skk*du) = I 
     dt     = du*inv(chol(du'*skk*du));
     temp   = inv(chol(du'*skk*du));
     

%      NOTE: At this point, the eigenvectors are aligned by column. To
%            physically move the column elements using the MATLAB sort,
%            take the transpose to put the eigenvectors across the row      

     dt     = transpose(dt);

% sort eigenvalues and vectors

     [au auind] = sort(diag(au)); 
     a = flipud(au);
     aind = flipud(auind);
     d = dt(aind,:);

%NOTE: The eigenvectors have been sorted by row based on auind and moved to array "d". 
%      Put the eigenvectors back in column format after the sort by taking the 
%      transpose of "d". Since the eigenvectors have been physically moved, there is 
%      no need for aind at all. To preserve existing programming, aind is reset back to 
%      1, 2, 3, ....

     d  =  transpose(d);
     test = transpose(d) * skk * d;

%EXPLANATION:  The MATLAB sort function sorts from low to high. The flip realigns
%auind to go from the largest to the smallest eigenvalue (now aind). The original procedure
%physically moved the rows of dt (to d) based on the alignment in aind and then used 
%aind as a column index to address the eigenvectors from high to low. This is a double 
%sort. If you wanted to extract the eigenvector corresponding to the largest eigenvalue by, 
%using aind as a reference, you would get the correct eigenvector, but with sorted 
%coefficients and, therefore, any follow-on calculation would seem to be in error.
%If alternative programming methods are used to evaluate the eigenvalues, e.g. Frame method
%followed by a root extraction on the characteristic equation, then the roots can be
%quickly sorted. One by one, the corresponding eigenvectors can be generated. The resultant
%array can be operated on using the Cholesky transformation, which enables a unit
%diagonalization of skk. But nowhere along the way are the coefficients within the
%eigenvector array ever changed. The final value of the "beta" array using either method
%should be the same.


% Compute the trace and max eigenvalue statistics */
     lr1 = zeros(m,1);
     lr2 = zeros(m,1);
     cvm = zeros(m,3);
     cvt = zeros(m,3);
     iota = ones(m,1);
     [t junk] = size(rkt);
     for i=1:m;
        tmp = trimr(log(iota-a),i-1,0);
        lr1(i,1) = -t*sum(tmp);
        lr2(i,1) = -t*log(1-a(i,1));
        cvm(i,:) = c_sja(m-i+1,p);
        cvt(i,:) = c_sjt(m-i+1,p);
        aind(i)  = i;
     end;
     
% set up results structure
result.eig = a;
result.evec = d;
result.lr1 = lr1;
result.lr2 = lr2;
result.cvt = cvt;
result.cvm = cvm;
result.ind = aind;
result.meth = 'johansen';


