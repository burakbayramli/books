function [u, dt, rms, u_min, u_max] = explicit_timestep ( u, res, cfl, a_max, NC )

h = 1/NC ; % mesh width
A = h^2 ; % cell area
dt = cfl*h/a_max ;% maximally allowable time-step
%% Time discretisation/Update
% track the rms of the residuals to measure convergence
rms = 0. ;
for i=1:NC
for j=1:NC
%% Explicit time-stepping
u(i,j)=u(i,j)-dt/A*res(i,j) ;
%% monitor convergence, use the root mean
%% square of the residual
rms = rms + res(i,j)^2 ;
endfor
endfor
## root mean square of the residual.
rms = sqrt(rms/NC^2) ;
u_min = min(min(u)) ;
u_max = max(max(u)) ;
end
