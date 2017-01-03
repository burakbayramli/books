function [Fclock,Qclock,Hclock,P0clock] = ClockModel2(sigmadrift,taudrift)
%
% Calculates the model parameters for a 2-state GNSS receiver clock in
% continuous time representation
%
% INPUTS
%
%   sigmadrift = standard deviation of clock drift rate, [m/s]
%   taudrift   = correlation time-constant of clock drift rates [s]
%
% OUTPUTS
%
%   Fclock  = 2x2 dynamic coeffient matrix
%   Qclock  = 2x2 process noise covariance matrix
%   Hclock  = 1x2 pseudorange sensitivity matrix
%   P0clock = initialvalue of covariance of uncertainty
%
%%  
%%  Grewal, Andrews, and Bartone,
%%  Global Navigation Satellite Systems, Inertial Navigation Systems, and Integration
%%  John Wiley & Sons, 2012.
%%  
Fclock  = [0,1;0,-1/taudrift];
Qclock  = [0,0;0,2*sigmadrift^2/taudrift];
Hclock  = [1,0];
P0clock = [10,0;0,sigmadrift^2];