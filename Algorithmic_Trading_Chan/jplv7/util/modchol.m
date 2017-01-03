function [l,d]=modchol(A)
% PURPOSE: Modified Cholesky algorithm of Elizabeth Eskow and Robert B. Schnabel
%          Performs a modified cholesky factorization
%          of a SYMMETRIC MATRIX A, of the form P'*A*P + d = L*L'
% -----------------------------------------------------------------------------
% USAGE: [l,d] = modchol(A)
% where: A = symmetric input matrix
%        L = cholesky matrix
%        d = the minimal diagonal increment according to the Gerschgorin Circle Theorem
% ------------------------------------------------------------------------------
% NOTES: Useful for dealing with ill-conditioned numerical hessians and the like
% Schnabel, R. B. and Eskow, E. 1990. "A New Modified Cholesky Factorization." 
% SIAM Journal of Scientific Statistical  Computing 11, 1136-58.) 
%
% Altman, M., J. Gill and M. P. McDonald.  2003.  Numerical Issues in Statistical
% Computing for the Social Scientist}.  John Wiley \& Sons, has a nice
% discussion of this and R-code for this algorithm at:
% http://www.hmdc.harvard.edu/numerical_issues/
% ------------------------------------------------------------------------------

% Jim LeSage

n=max(size(A));
gamma=max(abs(diag(A)));
psi=max(max(abs(A-diag(diag(A)))));
delta=eps*max(gamma+psi,1);
Bet=sqrt( max([gamma,psi/sqrt(n^2-1),eps]));
for k=1:n
c(k,k)=A(k,k);
end;
[q,dummy]=max(abs(diag(c)));
for j=1:n %compute jth col of L
	l(j,j)=1;
	for s=1:j-1
	l(j,s)=c(j,s)/d(s);
	end
	for i=j+1:n
	lc=0;
		for s=1:j-1
		lc=lc+l(j,s)*c(i,s);
		end;
	c(i,j)=A(i,j)-lc;
	end
	theta(j)=0;
	if (j <=n)
	mc=0;
		for i=j+1:n
			if abs(c(i,j)) >mc
			mc= abs(c(i,j)) ;
			end
			theta(j)=mc;
		end
	end;
	tt=[abs(c(j,j)),(theta(j)/Bet)^2,delta];
	d(j)=max(tt);
	if (j<n)
		for i=j+1:n
		c(i,i)=c(i,i)-c(i,j)^2/d(j);
		end;
	end;
end 


