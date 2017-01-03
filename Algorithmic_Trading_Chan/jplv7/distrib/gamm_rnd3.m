function out = gamm_rnd(n,a)
% PURPOSE: a vector of random draws from the gamma distribution
%---------------------------------------------------
% USAGE: r = gamm_rnd(n,a)
% where: n = the row size of the n x 1 vector drawn 
%        a = a parameter such that the mean of the gamma = a
%            and the variance of the gamma = a
%   notes: 
%   Mean = a;
%   Variance = a;
%   Skewness = 2/sqrt(a);
%   Kurtosis = 6/a;
%   Mode = a-1;
%        For different parameters, a,b use:
%	b*gamm_rnd(n,a) to produce a vector of random deviates from the gamma
%	distribution with shape parameter a and scale parameter b.  
%   The distribution then has mean a*b and variance a*b^2.
%   x = gamm_rnd(n,a*0.5)*2,equals chisq a random deviate
%---------------------------------------------------
% RETURNS:
%        r = an n x 1 vector of random numbers from
%        the gamma(a) distribution      
% --------------------------------------------------
% SEE ALSO: gamm_inv, gamm_pdf, gamm_cdf
%---------------------------------------------------

if a < 0
    error('gamm_rnd: shape parameter a must be > 0');
end;
                    
                    
if a < 1
% If a<1, one can use GAMMA(a)=GAMMA(1+a)*UNIFORM(0,1)^(1/a);

out = (feval('gamm_rnd',n, 1+a)).*(rand(n,1).^(1/a));                         
                         
else
                         
                         d = a - 1/3;
                         c = 1/sqrt(9*d);
                         
                         x = randn( n,1 );
                         v = 1+c*x;
                         
                         indxs = find(v <= 0);
                         while ~isempty(indxs)
                              indxsSize = size( indxs );
                              xNew = randn( indxsSize,1 );
                              vNew = a+c*xNew;
                              
                              l = (vNew > 0);
                              v( indxs( l ) ) = vNew(l);
                              x( indxs( l ) ) = xNew(l);
                              indxs = indxs( ~l );
                         end
                         
                         u = rand( n,1 );
                         v = v.^3;
                         x2 = x.^2;
                         out = d*v;
                         
                         indxs = find( (u>=1-0.0331*x2.^2) & (log(u)>=0.5*x2+d*(1-v+log(v))) );
                         while ~isempty(indxs)
                              indxsSize = size( indxs,1 );
                              
                              x = randn( indxsSize,1 );
                              v = 1+c*x;
                              indxs1 = find(v <= 0);
                              while ~isempty(indxs1)
                                   indxsSize1 = size( indxs1 );
                                   xNew = randn( indxsSize1,1 );
                                   vNew = a+c*xNew;
                                   
                                   l1 = (vNew > 0);
                                   v( indxs1(l1) ) = vNew(l1);
                                   x( indxs1(l1) ) = xNew(l1);
                                   indxs1 = indxs1( ~l1 );
                              end
                              
                              u = rand( indxsSize,1 );
                              v = v .* v .* v;
                              x2 = x.*x;
                              
                              l = (u<1-0.0331*x2.*x2) | (log(u)<0.5*x2+d*(1-v+log(v)));
                              out( indxs( l ) ) = d*v(l);
                              indxs = indxs( ~l );
                         end % while ~isempty(indxs)
                                                  
end % if a < 1, else ...
