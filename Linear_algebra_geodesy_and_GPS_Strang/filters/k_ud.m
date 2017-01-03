function [x,P] = k_ud(x,P,H,b,var)
%K_UD   Kalman update, one measurement per call
%	     Observation variance: var

%Kai Borre and C.C. Goad 11-24-96
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/10/15  $

innovation = b-H'*x; % omc
HP = H'*P;
innovation_variance = HP*H+var;
K = HP'/innovation_variance;
x = x+K*innovation;
P = P-K*HP;
%%%%%%%% end k_ud.m  %%%%%%%%%%%%%%%%%%
