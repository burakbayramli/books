function res = Hybrid3_AdvecRes1d_Algorithm2(qi,flux,dflux,S,dx)
%
% PolynomialReconstruction-THINC-BVD implementation 
% for scalar advection Equations 
%   -- Algorithm 2 
%
%% 1. Right Flux: $u_{i+1/2}^{-}$ with WENO5
vmm = circshift(qi,[0 2]);
vm  = circshift(qi,[0 1]);
v   = circshift(qi,[0 0]);
vp  = circshift(qi,[0 -1]);
vpp = circshift(qi,[0 -2]);

% Numerical Flux at cell boundary, $u_{i+1/2}^{-}$;
qiph_W = ( 2*vmm - 13*vm + 47*v + 27*vp - 3*vpp)/60;

%% 2. Left Flux: $u_{i-1/2}^{+}$ with WENO5
umm = circshift(qi,[0 2]);
um  = circshift(qi,[0 1]);
u   = circshift(qi,[0 0]);
up  = circshift(qi,[0 -1]);
upp = circshift(qi,[0 -2]);

% Numerical Flux at cell boundary, $u_{i-1/2}^{+}$;
qimh_W = (-3*umm + 27*um + 47*u - 13*up + 2*upp)/60;

%% 3. THINC reconstruction
% Constants parameters
Beta=1.6; epsilon=1E-20; delta=1E-4;

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
TBV_T = min( cat(3,...
    abs(circshift(qiph_W,+1)-qimh_T)+abs(qiph_T-circshift(qimh_W,-1)),...
    abs(circshift(qiph_T,+1)-qimh_T)+abs(qiph_T-circshift(qimh_T,-1)),...
    abs(circshift(qiph_W,+1)-qimh_T)+abs(qiph_T-circshift(qimh_T,-1)),...
    abs(circshift(qiph_T,+1)-qimh_T)+abs(qiph_T-circshift(qimh_W,-1))),[],3);

TBV_W = min( cat(3,...
    abs(circshift(qiph_W,+1)-qimh_W)+abs(qiph_W-circshift(qimh_W,-1)),...
    abs(circshift(qiph_T,+1)-qimh_W)+abs(qiph_W-circshift(qimh_T,-1)),...
    abs(circshift(qiph_W,+1)-qimh_W)+abs(qiph_W-circshift(qimh_T,-1)),...
    abs(circshift(qiph_T,+1)-qimh_W)+abs(qiph_W-circshift(qimh_W,-1))),[],3);

%% 4. BVD Algorithm
condition = delta<C & C<(1-delta) & ((qip1-qi).*(qi-qim1))>0 & TBV_T<TBV_W;
qiph_W(condition)=qiph_T(condition); qL=circshift(qiph_W,0);
qimh_W(condition)=qimh_T(condition); qR=circshift(qimh_W,-1);

% Debug
% qL=circshift(qiph_W,0);
% qR=circshift(qimh_W,-1);

%% Compute Lax-Friedrichs numerical flux and update solution
LF = 0.5*(flux(qL)+flux(qR)-abs(dflux((qi+qip1)/2)).*(qR-qL)); % Lax friedrichs flux
res = (LF-circshift(LF,1))/dx - S(qi); % L = - df(q)/dx + S(q).