function [coef,lambda_plus,lambda_minus,infsup] = param_est(H,iH,prob_type)
% PARAM_EST determines adaptive stopping criteria for EST_MINRES
%  [coef,lambda_plus,lambda_minus,infsup] = param_est(H,ih,prob_type);
%   input
%                H      Lanczos Matrix
%               iH      current dimension
%        prob_type      saddle point problem identifier 
%   output
%             coef      stopping criteria coefficient
%      lambda_plus      minimum positive eigenvalue 
%      lambda_plus      maximim negative eigenvalue 
%           infsup      inf-sup constant (associated with prob_type)
% 
%   IFISS function: DJS; 2 January 2011.
% Copyright (c) 2010 D.J. Silvester, V. Simoncini 
eHh=sort(eig(H(1:iH+1,1:iH)'*H(1:iH+1,1:iH),H(1:iH,1:iH)'));
ieHh=find(eHh<0); lambda_minus=max(eHh(ieHh));
ieHh=find(eHh>0); lambda_plus=min(eHh(ieHh));
if strncmp(prob_type,'stokes',6),
%%% invert formula (6.30) on p.297 in [ESW2005]
theta2=1;
infsup=(lambda_minus.^2-lambda_minus*lambda_plus)/(lambda_plus*theta2);
coef=sqrt(2)/infsup;
else, error('Illegal call to function param_est!'), end
return