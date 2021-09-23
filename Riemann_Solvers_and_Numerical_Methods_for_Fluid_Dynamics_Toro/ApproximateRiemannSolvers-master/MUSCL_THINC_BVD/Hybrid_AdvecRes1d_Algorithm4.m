function res = Hybrid_AdvecRes1d_Algorithm4(qi,flux,dflux,S,dx)
%
% WENO5-THINC-BVD implementation for scalar advection Equations 
%   -- Algorithm 4 
%
%% 1. Right Flux: $u_{i+1/2}^{-}$ with WENO5
vmm = circshift(qi,[0 2]);
vm  = circshift(qi,[0 1]);
v   = circshift(qi,[0 0]);
vp  = circshift(qi,[0 -1]);
vpp = circshift(qi,[0 -2]);

% Polynomials
p0n = (2*vmm - 7*vm + 11*v)/6;
p1n = ( -vm  + 5*v  + 2*vp)/6;
p2n = (2*v   + 5*vp - vpp )/6;

% Smooth Indicators (Beta factors)
B0n = 13/12*(vmm-2*vm+v  ).^2 + 1/4*(vmm-4*vm+3*v).^2; 
B1n = 13/12*(vm -2*v +vp ).^2 + 1/4*(vm-vp).^2;
B2n = 13/12*(v  -2*vp+vpp).^2 + 1/4*(3*v-4*vp+vpp).^2;

% Constants
d0n = 1/10; d1n = 6/10; d2n = 3/10; epsilon = 1e-6;

% Alpha weights 
alpha0n = d0n./(epsilon + B0n).^2;
alpha1n = d1n./(epsilon + B1n).^2;
alpha2n = d2n./(epsilon + B2n).^2;
alphasumn = alpha0n + alpha1n + alpha2n;

% ENO stencils weigths
w0n = alpha0n./alphasumn;
w1n = alpha1n./alphasumn;
w2n = alpha2n./alphasumn;

% Numerical Flux at cell boundary, $u_{i+1/2}^{-}$;
qiph_W = w0n.*p0n + w1n.*p1n + w2n.*p2n;

%% 2. Left Flux: $u_{i-1/2}^{+}$ with WENO5
umm = circshift(qi,[0 2]);
um  = circshift(qi,[0 1]);
u   = circshift(qi,[0 0]);
up  = circshift(qi,[0 -1]);
upp = circshift(qi,[0 -2]);

% Polynomials
p0p = ( -umm + 5*um + 2*u  )/6;
p1p = ( 2*um + 5*u  - up   )/6;
p2p = (11*u  - 7*up + 2*upp)/6;

% Smooth Indicators (Beta factors)
B0p = 13/12*(umm-2*um+u  ).^2 + 1/4*(umm-4*um+3*u).^2; 
B1p = 13/12*(um -2*u +up ).^2 + 1/4*(um-up).^2;
B2p = 13/12*(u  -2*up+upp).^2 + 1/4*(3*u -4*up+upp).^2;

% Constants
d0p = 3/10; d1p = 6/10; d2p = 1/10; epsilon = 1e-6;

% Alpha weights 
alpha0p = d0p./(epsilon + B0p).^2;
alpha1p = d1p./(epsilon + B1p).^2;
alpha2p = d2p./(epsilon + B2p).^2;
alphasump = alpha0p + alpha1p + alpha2p;

% ENO stencils weigths
w0p = alpha0p./alphasump;
w1p = alpha1p./alphasump;
w2p = alpha2p./alphasump;

% Numerical Flux at cell boundary, $u_{i-1/2}^{+}$;
qimh_W = w0p.*p0p + w1p.*p1p + w2p.*p2p;

% Compute total boundary variations TBV for each cell
TBV_W = abs(circshift(qiph_W,+1)-qimh_W)+abs(qiph_W-circshift(qimh_W,-1));

%% 3. THINC reconstruction
% Constants parameters
Beta=1.8; epsilon = 1E-20;

% Initial Arrays      
% qi = q;  % : q_{ j }^{n},
qim1 = circshift(qi,+1); % : q_{j-1}^{n},
qip1 = circshift(qi,-1); % : q_{j+1}^{n}.

% Coeficients
qmin = min(cat(3,qim1,qip1),[],3);
qmax = max(cat(3,qim1,qip1),[],3)-qmin;
theta= sign(qip1-qim1);
C = (qi-qmin+epsilon)./(qmax+epsilon);
B = exp(Beta*theta.*(2*C-1));
A = (B/cosh(Beta)-1)/tanh(Beta);

% q_{i+1/2}^{-} and q_{i-1/2}^{+} reconstructions for Beta_s
qiph_T = qmin + 0.5*qmax.*(1+theta.*(tanh(Beta)+A)./(1+A*tanh(Beta)));
qimh_T = qmin + 0.5*qmax.*(1+theta.*A);

% Compute total boundary variations TBV for each cell
TBV_T = abs(circshift(qiph_T,+1)-qimh_T)+abs(qiph_T-circshift(qimh_T,-1));

%% 4. BVD Algorithm
condition = TBV_T < TBV_W;
qiph_W(condition)=qiph_T(condition); qL=circshift(qiph_W,0);
qimh_W(condition)=qimh_T(condition); qR=circshift(qimh_W,-1);

% Debug
% qL=circshift(qiph_W,0);
% qR=circshift(qimh_W,-1);

%% Compute Lax-Friedrichs numerical flux and update solution
LF = 0.5*(flux(qL)+flux(qR)-abs(dflux((qi+qip1)/2)).*(qR-qL)); % Lax friedrichs flux
res = (LF-circshift(LF,1))/dx - S(qi); % L = - df(q)/dx + S(q).