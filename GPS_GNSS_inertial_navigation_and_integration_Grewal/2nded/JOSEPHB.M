function [K,Pout] = josephb(z,R,H,P)
%
% Joseph "stabilized" Kalman filter measurement
% update as modified by Bierman.
%
T1 = sqrt(R);
T2 = H/T1;
T4 = P*T2';
T5 = T2*T4 + 1;
K  = T4/T5;
T7 = P - K*T4';
T8 = T7*T2';
Pout = T7 - T8*K' + K*K';

