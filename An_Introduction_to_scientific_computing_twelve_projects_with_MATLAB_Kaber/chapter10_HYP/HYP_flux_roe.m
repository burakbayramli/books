%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%=======================================
% Roe's approximate solver
% computation of the flux  Phi
% from the components of W=(rho, rho*U, E)
%=======================================

function Phi = HYP_flux_roe(w)

global gamma aL pL rhoL aR pR rhoR dum1 dum2 dum3;

%=============================================
%---- Initialization
%============================================ 
% computation of (rho, U, p)
usol = HYP_trans_w_usol(w);

% enthalpy  H=gamma/(gamma-1) p/rho+U^2/2;
htot = gamma/(gamma-1)*usol(3,:)./usol(1,:)+0.5*usol(2,:).^2;

% initialization of the flux Phi
     M=size(w,2);Phi=zeros(3,M-1);
     
   for j=1:M-1 % loop on the space components
%=============================================
%---- Roe's averaged variables 
%============================================  
  r=sqrt(usol(1,j+1)/usol(1,j));              % R_{j+1/2}
  rmoy=r*usol(1,j);                           % {bar rho}_{j+1/2}
  umoy=(r*usol(2,j+1)+usol(2,j))/(r+1);       % {bar U}_{j+1/2}
  hmoy=(r*htot(j+1)+htot(j))/(r+1);           % {bar H}_{j+1/2}
  amoy=sqrt((gamma-1.0)*(hmoy-0.5*umoy*umoy));  %{bar a}_{j+1/2}
  
  %--> useful variables to compute P_{j+1/2}^{-1} 
  alph1=(gamma-1)*umoy*umoy/(2*amoy*amoy);
  alph2=(gamma-1)/(amoy*amoy);
	
%=============================================
%---- computation of  |A_{j+1/2}| (W_{j+1}-W_j)
%============================================
% vector (W_{j+1}-W_j)
wdif = w(:,j+1)-w(:,j);

% matrix P^{-1}_{j+1/2}
Pinv = [0.5*(alph1+umoy/amoy), -0.5*(alph2*umoy+1/amoy),  alph2/2 ;...
        1-alph1,                   alph2*umoy,           -alph2   ;...
        0.5*(alph1-umoy/amoy), -0.5*(alph2*umoy-1/amoy),  alph2/2 ];
  
% matrix P_{j+1/2}
P  = [ 1,             1,              1;...
      umoy-amoy,        umoy,           umoy+amoy;...
      hmoy-amoy*umoy,   0.5*umoy*umoy,  hmoy+amoy*umoy ];

% flux Phi
Phi(:,j) = P*diag([abs(umoy-amoy), abs(umoy), abs(umoy+amoy)])*Pinv*wdif;
end

%=============================================
%---- computation of  
%      Phi=(F(W_{j+1}+F(W_j))/2-|A_{j+1/2}| (W_{j+1}-W_j)/2
%============================================

F  = HYP_trans_w_f(w);                     % computation of F from W
Phi=0.5*(F(:,1:M-1)+F(:,2:M))-0.5*Phi; % final value for Phi
