function [t,nt] = sorad(A,w)
%
%   [t,nt] = sorad(A,w)
%    
%      w = SOR parameter
%    
%      t = SOR spectral radius
%      nt = expected number of SOR iterations for 1 decimal
%

D = diag(diag(A));
LD = tril(A); L = LD - D; U = A - LD;

J = D\(D - A); e = eig(J); r = max(abs(e));
G = -LD\U; f = eig(G); s = max(abs(f));

T = (w*L + D)\((1-w)*D - w* U); h = eig(T); t = max(abs(h));

Jacobi_spectral_radius = r
sr = r^2; Predicted_GS_spectral_radius = sr
GS_radius = s
SOR_spectral_radius = t
nr = log(.1)/log(r); Jacobi_iterations = nr
ns = log(.1)/log(s); GS_iterations = ns
nt = log(.1)/log(t); SOR_iterations = nt
