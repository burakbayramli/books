function  result = raftery(runs,q,r,s)
% PURPOSE: MATLAB version of Gibbsit by Raftery and Lewis (1991)
%    calculates the # of draws needed in MCMC to estimate
%    the posterior cdf of the q-quantile
%    to within +/- r accuracy with probability s
% --------------------------------------------------------------------
% USAGE:  raftery(draws,q,r,s)
% where:  draws = draws from the sampler (= ndraws x nvar matrix)
%             q = quantile of the quantity of interest
%             r = level of precision desired
%             s = probability associated with r
% --------------------------------------------------------------------        
% RETURNS: a structure
%     result.meth  = 'raftery'
%     result(i).nburn = number of draws required for burn-in (variable i)
%     result(i).nprec = number of draws required to achieve r precision
%     result(i).kthin = skip parameter for 1st-order Markov chain
%     result(i).irl   = I-statistic from Raftery and Lewis (1992)
%     result(i).kind  = skip parameter sufficient to get independence chain
%     result(i).nmin  = # draws if the chain is white noise
%     result(i).n     = nburn + nprec
%     result.draws    = # of draws in draws input matrix
%     result.nvar     = # of variables in draws input matrix
%     result.q        = q-value (from input)
%     result.r        = r-value (from input)
%     result.s        = s-value (from input)
% ---------------------------------------------------------------------
% NOTES:   Example values of q, r, s:                                 
%     0.025, 0.005,  0.95 (for a long-tailed distribution)             
%     0.025, 0.0125, 0.95 (for a short-tailed distribution);           
%     0.5, 0.05, 0.95;  0.975, 0.005, 0.95;  etc.                                                                                            
%  - The result is quite sensitive to r, being proportional to the       
%  inverse of r^2.                                                                                                                          
%  - For epsilon, we have always used 0.001.  It seems that the result   
%  is fairly insensitive to a change of even an order of magnitude in  
%  epsilon.                                                                                                                                  
%  - One way to use the program is to run it for several specifications  
%  of r, s and epsilon and vectors q on the same data set.  When one   
%  is sure that the distribution is fairly short-tailed, such as when  
%  q=0.025, then r=0.0125 seems sufficient.  However, if one is not    
%  prepared to assume this, safety seems to require a smaller value of 
%  r, such as 0.005.                                                                                                                        
%  - The program takes as input the name of a file containing an initial 
%  run from a MCMC sampler.  If the MCMC iterates are independent,     
%  then the minimum number required to achieve the specified accuracy  
%  is about $\Phi^{-1} (\frac{1}{2}(1+s))^2 q(1-q)/r^2$ and this would 
%  be a reasonable number to run first.                                
%  When q=0.025, r=0.005 and s=0.95, this number is 3,748;             
%  when q=0.025, r=0.0125 and s=0.95, it is 600.  
% ---------------------------------------------------------------------
% SEE ALSO: coda(), prt(), raftery_d.m                            
% ---------------------------------------------------------------------
% REFERENCES: Raftery, A.E.  and Lewis, S.M.  (1992).  How many iterations 
% in the  Gibbs sampler?  In Bayesian Statistics, Vol.  4 (J.M.  Bernardo, 
% J.O.   Berger, A.P.  Dawid and A.F.M.  Smith, eds.).  Oxford, U.K.: Oxford 
% University Press, 763-773.   This paper is available via the World Wide 
% Web by linking to URL  
% http://www.stat.washington.edu/tech.reports/pub/tech.reports % and then 
% selecting the "How Many Iterations in the Gibbs Sampler" % link.                                                              
%  This paper is also available via regular ftp using the following commands:                                                           
%    ftp ftp.stat.washington.edu (or 128.95.17.34)                                                                                                                                                   *
%  Raftery, A.E. and Lewis, S.M. (1992).  One long run with diagnos-   
%  tics: Implementation strategies for Markov chain Monte Carlo.       
%  Statistical Science, Vol. 7, 493-497.                                                                                                    *
%  Raftery, A.E. and Lewis, S.M. (1995).  The number of iterations,    
%  convergence diagnostics and generic Metropolis algorithms.  In      
%  Practical Markov Chain Monte Carlo (W.R. Gilks, D.J. Spiegelhalter  
%  and S. Richardson, eds.). London, U.K.: Chapman and Hall.           
%  This paper is available via the World Wide Web by linking to URL    
%    http://www.stat.washington.edu/tech.reports/pub/tech.reports      
%  and then selecting the "The Number of Iterations, Convergence       
%  Diagnostics and Generic Metropolis Algorithms" link.                
%  This paper is also available via ftp.stat.washington.edu (or 128.95.17.34)                               
% ----------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

% NOTE: this is a translation of FORTRAN code
%       (which is why it looks so bad)
 
 [n nvar] = size(runs); 
    tmp  = zeros(n,1); 
    work = zeros(n,1);
 
result.meth = 'raftery';
result.draws = n;
result.nvar = nvar;
result.q = q;
result.r = r;
result.s = s;
    
    for nv = 1:nvar; % big loop over variables
  
 if (q > 0.0),
   cutpt = empquant(runs,q);
   work = (runs <= cutpt);
 else,
  q = 0.0;
  i1 = find(runs == 0); i2 = find(runs == 1);
  ct1 = size(i1);   ct2 = size(i2); 
 if (ct1+ct2 ~= n), error('raftery needs 0s and 1s in runs'); end;
 work = runs;
 q = sum(runs);
 q = q/n;
 end;      % end of if;
 
 kthin = 1; bic = 1.0; epss = 0.001;
  
 while(bic > 0);
 [tcnt tmp] = thin(work,n,kthin);
 [g2 bic] = mctest(tmp,tcnt);
 kthin = kthin+1; 
 end;      % end of while
 
 kthin = kthin-1; 
 [alpha beta]=mcest(tmp,tcnt);
 kmind=kthin; 
 [g2 bic]=indtest(tmp,tcnt);
  
 while(bic > 0);
 [tcnt tmp] = thin(work,n,kmind); [g2 bic] = indtest(tmp,tcnt);
 kmind = kmind + 1; 
 end;      % end of while
  
 psum  = alpha + beta;
 tmp1  = log(psum*epss/max(alpha,beta)) / log(abs(1.0 - psum));
 nburn = fix((tmp1+1.0)*kthin);
 phi   = ppnd((s+1.0)/2);
 tmp2  = (2.0 - psum)*alpha*beta*(phi^2)/(psum^3 * r^2);
 nprec = fix(tmp2+1.0)*kthin; 
 nmin  = fix(((1.0-q)*q*phi^2/r^2)+1.0);
 irl   = (nburn + nprec)/nmin; 
 kind  = max(fix(irl+1.0),kmind);

result(nv).nburn = nburn;
result(nv).nprec = nprec;
result(nv).kthin = kthin;
result(nv).kind = kind;
result(nv).irl = irl;
result(nv).nmin = nmin;
result(nv).n = nburn+nprec;

end; % end of big loop over variables
