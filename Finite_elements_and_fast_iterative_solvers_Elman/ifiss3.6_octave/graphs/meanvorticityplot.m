function meanv=meanvorticityplot(qmethod,ev,sol,tt,By,Bx,G,xy,fig,tout)
%MEANVORTICITYPLOT plots mean vorticity (in channel flow)
%     meanv = meanvorticityplot(qmethod,ev,sol,tt,By,Bx,G,xy,fig,tout);
%   input
%          qmethod    mixed method 
%          ev         mv/ev  Q2/Q1 element mapping matrix
%          sol        flow solution vector
%          tt         snapshot time vector
%          By         velocity  y-derivative matrix    
%          Bx         velocity x-derivative matrix    
%          G          velocity mass matrix
%          xy         velocity nodal coordinate vector  
%          fig        figure number
%          tout       output on/off switch (optional)
%   output
%          meanv      mean vorticity at every time step
%
%   IFISS function: DJS; 9 November 2014.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 

if nargin < 10, tout = 1; end
figure(fig)
nstep=length(tt);
meanv=zeros(nstep,1);
nvtx=length(xy); nu=2*nvtx; 
%[LG,UG]= lu(G(1:nvtx,1:nvtx));
if tout, fprintf('\n   step   mean_vorticity\n'), end
for k=1:nstep
% compute auxilliary quantites
u=sol(:,k);
fsv=-[By,-Bx]*u;
omega=G(1:nvtx,1:nvtx)\fsv; %UG\(LG\fsv);
%
%%%  
if qmethod > 1,    
   wev = vorticity_q2(xy,ev,omega,0);
else
   wev = vorticity_q1(xy,ev,omega,0);
end
if tout, fprintf('  %4i    %11.3e \n', k, sum(wev)); end
%
meanv(k)=sum(wev);
end
plot(tt,meanv,'.k'), axis square
hold on, plot(tt,meanv,'-b'), hold off
title('mean vorticity evolution')
fprintf('\nAll done\n')
fprintf('step %g : final time is %9.3e\n',k,tt(k))
if tout, fprintf('minimum w is %g ',min(omega))
fprintf('and maximum w is %g\n',max(omega)), end
return
