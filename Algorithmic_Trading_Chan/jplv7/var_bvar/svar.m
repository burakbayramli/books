function [a,b,a_se,b_se]=svar(results,sa,sb,da,db,afree,bfree)
% PURPOSE: svar verifies the identification conditions for a given structural form
%          to be imposed on an estimated Var model; if the model is exactly 
%          or overidentified, svar performs a FIML estimation of the
%          structural parameters. 
%-----------------------------------------------------------------------
% USAGE:  [a,b,a_se,b_se] = svar(results,sa,sb,da,db,afree,bfree) 
% ----------------------------------------------------------------------
% INPUT:
% sa,sb,da,db:      the matrix of the constraints in explicit form
%                   so that: vec(A)=sa*gammaa+da and vec(B)=sb*gammab+db;
% afree:            # of free parameters in A matrix
% bfree:            # of free parameters in B matrix
% results:          a Vare structure;
% -----------------------------------------------------------------------
% OUTPUT:
% a:                estimated A matrix;
% b:                estimated B matrix:
% a_se:             standard errors of a;
% b_se:             standard errors of b;
% -------------------------------------------------------------------------
% NOTE: modeled after RATS svar function
% -------------------------------------------------------------------------
% SEE ALSO: vare.m. 
% ------------------------------------------------------------------------
% REFERENCES: 
% Amisano and Giannini (1994). Topics in structural Var econometrics. 
% Favero, C. A. (2000). Applied Macroeconometrics. Oxford University Press, forthcoming.
% -------------------------------------------------------------------------

% Written by:
% Marco Aiolfi
% maiolfi@iol.it
% Bocconi University and Banca Intesa, Milan

if isstruct(results)
   n = results(1).neqs;
   T = results(1).nobs;
   for i = 1:n
      err(:,i)=results(i).resid;
   end
   sigma = err'*err/rows(err); %Var-Cov matrix of VAR
else
   error('YOU MUST INPUT A VAR RESULTS STRUCTURE')
end


nsq=n^2;
a=zeros(n);b=zeros(n);
l=afree+bfree;
s=zeros(2*nsq,l);
d=zeros(2*nsq,1);
% Compute commutation matrix
tenx=commutation(n,n);
for i=1:nsq
   for j=1:afree
      s(i,j)=sa(i,j);
      s(i+nsq,j)=0;
   end
   for j=afree+1:l
      s(i,j)=0;
      s(i+nsq,j)=sb(i,j-afree);
   end
   d(i)=da(i);
   d(i+nsq)=db(i);
end
% Check identification conditions referring to the
% rank of the so called augmented information matrix
mid=eye(n);
mid2=eye(nsq);
% gamma=RANDOM('Normal',0,1,l,1); % replaced by LeSage
gamma = randn(l,1);

vecab=s*gamma+d;
a=reshape(vecab(1:nsq),n,n);
b=reshape(vecab(nsq+1:2*nsq),n,n);
k=inv(b)*a;
idmat1=(mid2+tenx)*kron(inv(k'),inv(b))*sa;
idmat2=-(mid2+tenx)*kron(mid,inv(b))*sb;
size(idmat1);size(idmat2);
idmat=[idmat1 idmat2];
ms=idmat'*idmat;
[r,c]=size(ms);
e=eig(ms);
rni=0;
for i=1:c
   if e(i)<1e-10
      rni=rni+1;
   end
end
rni
if rni==0
   if l==n*(n+1)/2
      display('The model is just identified')
   else
      display('the model is over-identified')
   end
else
   display('The model is not identified')
end

%========================================
maxit=1000; % max number of iterations
tol=.1e-6; %tolerance
maxls=1;
%========================================
gamma0=zeros(l,1);
for i=1:length(gamma0)
   gamma0(i)=0.1;
end
mid=eye(n);
mid2=eye(nsq);
%neqs=results.neqs;
%T=results.nobs;
vecab=zeros(2*nsq,1);

% FIML estimation of the structural parameters using the 
% scoring algorithm

for i=1:maxit;
   vecab=s*gamma0+d;
   a=reshape(vecab(1:nsq),n,n);
   b=reshape(vecab(nsq+1:2*nsq),n,n);
   k=inv(b)*a;
   ktmu=(inv(k))';
   derk1=kron(mid,inv(b));
   derk2=-kron(k',inv(b));
   derk=[derk1 derk2];
   infk=kron(inv(k),mid)*(mid2+tenx)*kron(ktmu,mid)*T;
   infgamma=s'*derk'*infk*derk*s;
   fveck=T*(vec(ktmu)-kron(sigma,mid)*vec(k));
   fgamma=s'*derk'*fveck;
   dir=inv(infgamma)*fgamma;
   len=max(abs(dir));
   if len>maxls
      lambda=maxls/len;  % set "step length"
   else
      lambda=1;
   end
   gamma1=gamma0+lambda*dir;
   z=gamma1-gamma0;
   if max(abs(z))<=tol
         disp(['Convergence achieved after ',num2str(i),' iterations'])
         break  
   else
      gamma0=gamma1;
   end
   if i==maxit
      display(['Covergence not achieved after ',num2str(maxit),' iterations'])
      break
   end
end
gamma=gamma1;

% build matrix a and b
veca=sa*gamma(1:afree)+da; a=reshape(veca,n,n);
vecb=sb*gamma(afree+1:l)+db; b=reshape(vecb,n,n);
% display('Estimated A matrix');
a;
% display('Estimated B matrix');
b;
% compute standard errors
absigma=s*inv(infgamma)*s'/T;
absigma=sqrt(diag(absigma));
a_se=reshape(absigma(1:nsq),n,n);
b_se=reshape(absigma(nsq+1:2*nsq),n,n);