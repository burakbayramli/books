%Initialize variables 
%
default    = 'y';				      	% use default satellite set 
tstep      = 10;						   % time step
tstop      = 3600;					  	% time stop
gdop_out   = zeros(1,tstop/tstep);	%improve execution time by defining variable sizes
Pminus_out = zeros(5,tstop/tstep);
Pplus_out  = zeros(5,tstop/tstep);
Kbar_out   = zeros(5,4,tstop/tstep);
time       = zeros(1,tstop/tstep);
phi        = eye(5);
phi(4,5)   = 1;
% *******************************************************************************
% Initialize initial covariance or estimation error P0_plus
%
P0_plus      = zeros(5);
P0_plus(1,1) = 10000;
P0_plus(2,2) = 10000;
P0_plus(3,3) = 10000;
P0_plus(4,4) = 90000;
P0_plus(5,5) = 900;
Pplus_last   = P0_plus;
% **************************************************************************
% Initialize dynamic disturbance covariance matrix Q
%
Q      = zeros(5);
Q(1,1) = .333;
Q(2,2) = .333;
Q(3,3) = .333;
Q(4,4) = .0833;
Q(5,5) = .142;
% **************************************************************************
% Initialize pseudorange noise covariance matrix R
%
R      = zeros(4,4);
R(1,1) = 225;
R(2,2) = 225;
R(3,3) = 225;
R(4,4) = 225;
% *************************************************************************
% Initialize measurement sensitivity matrix H
%
H      = zeros(4,5);
H(:,4) = 1;
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  
