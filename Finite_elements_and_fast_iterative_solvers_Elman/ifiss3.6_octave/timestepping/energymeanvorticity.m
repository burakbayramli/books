function [ke,acc,meanv]=energymeanvorticity(qmethod,ev,U,Udot,tt,By,Bx,G,xy,tout,fig)
%ENERGYMEANVORTICITY compute acceleration and mean vorticity
%     [ke,acc,meanv] = energymeanvorticity(qmethod,mv,U,Udot,xtime,By,Bx,G,xy,1,101);
%   input
%          qmethod    mixed method 
%          ev         mv/ev  Q2/Q1 element mapping matrix
%          U          flow solution vector
%          Udot       acceleration vector
%          tt         snapshot time vector
%          By         velocity  y-derivative matrix    
%          Bx         velocity x-derivative matrix    
%          G          velocity mass matrix
%          xy         velocity nodal coordinate vector  
%          tout       output on/off switch (optional)
%          fig        figure number
%   output
%          ke         kinetic energy (at every time step)
%          acc        acceleration (at every time step)
%          meanv      mean vorticity (at every time step)
%
%   IFISS function: DJS; 9 August 2016.
% Copyright (c) 2016 D.J. Silvester, H.C. Elman

if nargin < 9, tout = 0; end
nstep=length(tt);
meanv=zeros(nstep,1); ke=zeros(nstep,1);
nvtx=length(xy); nu=2*nvtx; 
%[LG,UG]= lu(G(1:nvtx,1:nvtx));
%%% debug
%fprintf('\n   step   mean_vorticity   acceleration\n')

%--- timestep loop
for k=1:nstep
% compute auxilliary quantites
u=U(:,k); udot=Udot(:,k);
fsv=-[By,-Bx]*u;
omega=G(1:nvtx,1:nvtx)\fsv; %UG\(LG\fsv);
%
%%%  
if qmethod > 1,    
   wev = vorticity_q2(xy,ev,omega,0);
else
   wev = vorticity_q1(xy,ev,omega,0);
end
stepke=sqrt(u'*G*u); stepacc=sqrt(udot'*G*udot);
%%% debug
%fprintf('  %4i    %11.3e   %11.3e\n', k, sum(wev), stepacc);
meanv(k)=sum(wev); ke(k)=stepke; acc(k)=stepacc;
end
%---- end of timestep loop
if tout, figure(fig),
subplot(121)
plot(1:nstep,meanv,'oc'), axis square
hold on, plot(1:nstep,meanv,'-b'), hold off
title('mean vorticity evolution')
xlabel('timestep')
%xlabel('time')
subplot(122)
semilogy(1:nstep,acc,'-k'), axis square
hold on, semilogy(1:nstep,acc,'ob'), hold off
title('flow acceleration'), xlabel('timestep'), pause(1)
%%% debug
fprintf('step %g : final time is %9.3e\n',k,tt(k))
end

fprintf('minimum energy is %g ',min(ke))
fprintf('and maximum is %g\n\n',max(ke)),
return

