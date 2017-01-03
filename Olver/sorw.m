function [w,r,s,t,nr,ns,nt] = sorw(A)
%
%   w = sorw(A)
%    
%      Compute optimal SOR relaxation parameter w for A*x = b
%    
%      r = Jacobi spectral radius
%      s = Gauss-Seidel spectral radius
%      t = SOR spectral radius
%      nr = expected number of Jacobi iterations for 1 decimal
%      ns = expected number of Gauss-Seidel iterations for 1 decimal
%      nt = expected number of SOR iterations for 1 decimal
%

D = diag(diag(A));
LD = tril(A); L = LD - D; U = A - LD;

J = D\(D - A); e = eig(J); r = max(abs(e));
G = -LD\U; f = eig(G); s = max(abs(f));

w = 2/(1 + sqrt(1 - r^2))
T = (w*L + D)\((1-w)*D - w* U); h = eig(T); t = max(abs(h));

Jacobi_spectral_radius = r
sr = r^2; Predicted_GS_spectral_radius = sr
GS_radius = s
tw = w - 1; Predicted_SOR_spectral_radius = tw
SOR_spectral_radius = t
nr = log(.1)/log(r); Jacobi_iterations = nr
ns = log(.1)/log(s); GS_iterations = ns
nt = log(.1)/log(t); SOR_iterations = nt
