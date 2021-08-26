function [Pu] = SingularFourierPade(x,u,z,N0,Nc);
% function [Pu] = SingularFourierPade(x,u,N);
% Purpose: Apply Singular Fourier-Pade postprocessing to periodic function 
%    p_M(x)/q_L(x) + r_R(x)/Q_L(x) = u_Nc(x) + x^(Nc+1). u_N is assumed real
% Approach follows Driscoll and Fornberg (2001) and
% padelog.m (Driscoll, MathWorks File Exchange, 2006)

% Extract Nc Fourier coefficients and compute denominator
uh = fft(u)/(2*N0+1); uhat = uh(1:Nc+1); N=Nc;

% Determine parameters and location of discontinuities in [0,2pi]
Nx = length(x); xl = 2*pi/Nx*[0:Nx-1]';
ii = sqrt(-1.0); xp = exp(ii*xl); xm =exp(-ii*xl);

xmin = min(x); xmax = max(x); xi = (z-xmin)/(xmax-xmin)*2*pi;
xip = exp(ii*xi); xim = exp(-ii*xi); mm = length(z);

% Setting orders of polynomials -- these can be adjusted
L = ceil((N-mm)/(mm+1.5))-1; s = floor((N-mm-L)/(mm+1))+1;
R = s*ones(1,mm); M = N-mm-L-sum(R);

% Taylor coeffs of log terms
k = (1:N)';
for s = 1:mm
  lp{s} = [0;-1./(k.*xip(s).^k)];
end

% The polynomials Q and R{:} are found from the highest-order coeffs
row = [uhat(M+2:-1:max(1,M-L+2)); zeros(L-M-1,1)];
Cq = toeplitz(uhat(M+2:N+1),row);
Lp = cell(1,mm);
for s = 1:mm
  row = [lp{s}(M+2:-1:max(1,M-R(s)+2));zeros(R(s)-M-1,1)];
  Lp{s} = toeplitz(lp{s}(M+2:N+1),row);
end

% Find a vector v satisfying [Cq -Lp{1} ... -Lp{m}]*v = 0
Z = null(cat(2,-Cq,Lp{:}));
qr = Z(:,end); qr = qr/qr(min(find(qr)));

% Pull out polynomials
qp = qr(1:L+1); 
idx = L+1; rp = cell(1,mm); rm = cell(1,mm);
for s = 1:mm
  rp{s} = qr(idx+(1:R(s)+1)); rm{s} = conj(rp{s});
  idx = idx + R(s)+1;
end

% Remaining polynomial is found using low-order terms
Cq = uhat(1:M+1); Cq(1) = Cq(1)/2; Rq = zeros(1,M+1); Rq(1) = Cq(1);
A = toeplitz(Cq,Rq);
pp = A*qp(1:M+1);
for s = 1:mm
  Lp = toeplitz(lp{s}(1:M+1),[lp{s}(1) zeros(1,R(s))]);
  pp = pp - Lp*rp{s};
end
pm = conj(pp); qm = conj(qp); 

% Evaluate singular Pade-Fourier form
qpp = polyval(flipud(qp),xp); qmp = polyval(flipud(qm),xm);
Pu = polyval(flipud(pp),xp)./qpp + polyval(flipud(pm),xm)./qmp;

for s=1:mm
    rpp = polyval(flipud(rp{s}),xp); rpm = polyval(flipud(rm{s}),xm);
    Pu = Pu + rpp.*log(1-xp/xip(s))./qpp + rpm.*log(1-xm/xim(s))./qmp;
end
return