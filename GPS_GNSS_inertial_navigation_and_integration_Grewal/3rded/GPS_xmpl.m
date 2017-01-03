% GPS_xmpl.m 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Covariance analysis of expected performance of a GPS receiver %
% using a Kalman filter and selected satellite geometries.      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
clear all;
close all;
init_var;					% Initialization parameters and variables
choose_sat;					% Choose satellite set or use default
gps_initX;					% Initialize GPS satellites selection
for k=1:tstop/tstep;	
   t=k*tstep;				% Calculate time for iteration k
   calcH;					% Calculate H matrix
   gdop;					% Calculate GDOP for chosen constellation
   covar;					% Solve Riccati equation
end;
plot_covar;					% Plots
%%
%%  For Problem 10.7 in
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and
%%  Integration, 3rd edition, Wiley-Interscience, 2013.
%%  
