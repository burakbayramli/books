function [Pu] = FourierPade(x,u,N0,Nc,M,L);
% function [Pu] = FourierPade(x,u,N,M,L);
% Purpose: Apply Fourier-Pade postprocessing to periodic function with
%    p_M(x)/q_L(x) = u_Nc(x) + x^(Nc+1). u_N is assumed real
% Approach follows Driscoll and Fornberg (2001).
% NOTE: M+L=Nc is assumed

% Extract Nc Fourier coefficients and compute denominator
uh = fft(u)/(2*N0+1); uhat = uh(1:Nc+1); N=Nc;

Cq = uhat(M+2:N+1); Rq = [uhat(M+2:-1:max(1,M-L+2)); zeros(L-M-1,1)];
Z = null(toeplitz(Cq,Rq));
qp = Z(:,end); qp = qp/qp(min(find(qp))); qm = conj(qp);

% Compute numerator
Cq = uhat(1:M+1); Cq(1) = Cq(1)/2;
Rq = zeros(1,M+1); Rq(1) = Cq(1);
A = toeplitz(Cq,Rq);
pp = A*qp(1:M+1); pm = conj(pp);

% Evaluate Pade-Fourier approximation
Nx = length(x); xl = 2*pi/Nx*[0:Nx-1]';
ii = sqrt(-1.0); xp = exp(ii*xl); xm =exp(-ii*xl);

Pu = polyval(flipud(pp),xp)./polyval(flipud(qp),xp) ...
    + polyval(flipud(pm),xm)./polyval(flipud(qm),xm);
return