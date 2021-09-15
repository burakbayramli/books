% How many cells in each coordinate direction should be used?
%NC=input(' Number of cells NC in each dir = ?');
NC = 10
if isempty ( NC ) NC = 4 ; end
% How many timesteps
%nt=input(' Number of timesteps nt= ?');
nt=10
if isempty ( nt ) nt = 1 ; end
% Choose a CFL number
%cfl=input(' CFL number cfl= ') ;
cfl=1.0
% maximum advection speed in the top left corner limits the timestep
if isempty ( cfl ) cfl = 0.5 ; end
%% maximal advection speed is found in the top left corner,
%% this limits the time-step in the explicit time-stepping.
a_max = sqrt(2) ;
# intialise the field.
u = initial_solution ( NC ) ;
%% Index n loops over all timesteps nt
t_total = 0.0 % time covered by the simulation.
nplot = -1 ; % number of plots of the field so far.
for n=1:nt
  res = residual ( u, NC ) ;
  [u, dt, rms, u_min, u_max] = explicit_timestep ( u, res, cfl, a_max, NC ) ;
  t_total = t_total + dt ;
  fprintf ( ' iter %3d, time %6.3f, log of RMS: %7.3f, u_min %6.3f, u_max %6.3f.\n',n, t_total, log10(rms), u_min, u_max ) ;
  mplot = 5 ;
  if ( floor( mplot*t_total/pi*2 ) > nplot )
    nplot = nplot+1 ;

    hold off
    plot_field ( u, NC, 1 ) ;
    
    hold on ;
    title ( sprintf ( 'solution at time t=%6.3f', t_total ) ) ;
    pause ( 0.02 ) ;
  end
end

plot_field ( u, NC, 1 ) ;
title ( sprintf ( 'circular advection, %dx%d cells',NC,NC ) ) ;
pause ;
print ( '-dpng','circular_advection_field.png') ;

plot_field ( u, NC, 3 ) ;
print ( '-dpng','circular_advection_prof.png') ;
pause ;
