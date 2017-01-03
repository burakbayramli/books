function [x,P] = k_update(x,P,A,b,R)
% K_UPDATE  Kalman update, one measurement per call
%	         Observation covariance R

%Kai Borre 11-24-96
%Copyright (c) 1997 by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/22  $

omc = b-A*x;
AP = A*P;
innovation_variance = AP*A'+R;
K = AP'/innovation_variance;
x = x+K*omc;
P = P-K*AP;
%%%%%%%% end k_update.m  %%%%%%%%%%%%%%%%%%
