% PURPOSE: Demonstrate pow_min optimization function 
%          using the Banana function
% f(x,y)=-100*(y-x*x)^2-(1-x)^2, starting at [0; 0]. 
%---------------------------------------------------
% USAGE: pow_mind
%---------------------------------------------------

clear all;
parmx = 0;
parmy = 0;
parm = [parmx
        parmy];

info.pflag = 1; % turn on intermediate printing options

result = pow_min('banana',parm,info);
disp('time taken by pow_min routine');
mprint(result.time);

fprintf(1,'parameter values \n');
mprint(result.b);

fprintf(1,'hessian matrix \n');
mprint(result.hess);

fprintf(1,'# of iterations \n');
result.iter

fprintf(1,'function value at optimum \n');
mprint(result.f);
