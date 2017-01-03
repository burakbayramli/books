% PURPOSE: Demonstrate frpr_min optimization function 
%          using the Banana function
% f(x,y)=-100*(y-x*x)^2-(1-x)^2, starting at [0; 0]. 
%---------------------------------------------------
% USAGE: frpr_mind
%---------------------------------------------------

clear all;
parmx = 0;
parmy = 0;
parm = [parmx
        parmy];

info.pflag = 0; % turn on intermediate printing options

result = frpr_min('banana',parm,info);
disp('time taken by frpr_min routine');
mprint(result.time);

fprintf(1,'parameter values \n');
mprint(result.b);

fprintf(1,'hessian matrix \n');
mprint(result.hess);

fprintf(1,'# of iterations taken \n');
mprint(result.iter);

fprintf(1,'function value at optimum \n');
mprint(result.f);

