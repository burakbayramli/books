function [x,P] = b_row(x,P,A,b,R)
%B_ROW  Bayes update, one measurement per call
%	     Observation covariance R

%Kai Borre 3-28-97
%Copyright (c) 1997 by Kai Borre
%$Revision: 1.1 $  $Date: 1997/11/22  $

omc = b-A*x;
ATR_inv = A'*inv(R);
P = inv(inv(P)+ATR_inv*A);
K = P*ATR_inv;
x = x+K*omc;
%%%%%%%% end b_row.m  %%%%%%%%%%%%%%%%%%
