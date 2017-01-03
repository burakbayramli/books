function [x,P,K,innovation_variance] = k_updatx(x,P,A,b,R,Q);
% K_UPDATX   Kalman update, one measurement per call
%	          Allows for system covariance Q
%	          Allows for observation covariance R

%Kai Borre 12-18-96
%Copyright (c) Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/22  $

omc = b-A'*x;
P = P + Q;
AP = A'*P;
innovation_variance = AP*A+R;
K = AP'/innovation_variance;
x = x+K*omc;
P = P-K*AP;
%%%%%%%% end k_updatx.m  %%%%%%%%%%%%%%%%%%
