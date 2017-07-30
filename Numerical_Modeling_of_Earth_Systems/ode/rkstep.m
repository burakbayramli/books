function dy = rkstep(.... )
%
%
% perform one 4th order Runge Kutta timestep and return
% the increment on y(t_n) by evaluating func(time,y,parameters) 
%
% ... parts need to be filled in 
%
%
% input values:
%  h: time step
%  t: time 
%  y: vector with variables at time = t which are to be advanced
%  func: function which computes dy/dt
%  parameters: structure with any parameters the func function might need

% save computations
h2=h/2;

k1 = h .* dydt(...);
k2 = h .* dydt(...);
....
% return the y_{n+1} timestep
dy =  ....
    
