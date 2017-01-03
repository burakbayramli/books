function cc1 = garch_trans(cc0)
% PURPOSE: function to transform garch(1,1) a0,a1,a2 garch parameters
% -----------------------------------------------------
% USAGE: out = garch_trans(in);
% where: in is a 3x1 vector with a0,a1,a2 parameters
% -----------------------------------------------------
% RETURNS: a 3x1 vector with:
% a0^2;
% a1=exp(a0)/(1 + exp(a1) + exp(a2));
% a2=exp(a0)/(1 + exp(a1) + exp(a2));
% -----------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com


k = length(cc0) - 3;
    cc1=cc0;

    cc1(k+1,1)=(cc0(k+1,1))^2;
    cc1(k+2,1)=exp(cc0(k+2,1))/(1 + exp(cc0(k+2,1)) + exp(cc0(k+3,1)));
    cc1(k+3,1)=exp(cc0(k+3,1))/(1 + exp(cc0(k+2,1)) + exp(cc0(k+3,1)));
