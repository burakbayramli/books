function [u] = KPPC2D(x,y,u,hx,hy,CFL,FinalTime)
% function [u] = KPPC2D(u,hx,hy,CFL,FinalTime)
% Purpose  : Integrate 2D KPP equation until FinalTime using second order central scheme.
time = 0; tstep = 0;

% integrate scheme
k = CFL*min(hx,hy)/2; maxvel=1;
while (time<FinalTime)
   % Decide on timestep
   if (time+k>FinalTime) k = FinalTime-time; end
   u = u + k*KPPCrhs2D(x,y,u,hx,hy,k,maxvel); 
   time = time+k; tstep = tstep+1;
end
return