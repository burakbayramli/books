% GPS_perf.m
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Covariance analysis of expected performance of a GPS receiver %
% using a Kalman filter.                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
clear all;
close all;
init_var;					% Initialization parameters and variables
choose_sat;					% Choose satellite set or use default
gps_init;					% Initialize GPS satellites
for k=1:tstop/tstep;	
   t=k*tstep;				% Calculate time for iteration k
   calcH;					% Calculate H matrix
   gdop;						% Calculate GDOP for chosen constellation
   covar;					% Solve Riccati equation
end;
plot_covar;					% Plots