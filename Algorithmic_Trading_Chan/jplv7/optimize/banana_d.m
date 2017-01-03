% PURPOSE: Demonstrate optimization functions 
%          using the Banana function
% f(x,y)=-100*(y-x*x)^2-(1-x)^2, starting at [0;1]. 


parmx = 0;
parmy = 0;
parm = [parmx
        parmy];

info.pflag = 0; % turn on intermediate printing options

res1 = frpr_min('banana',parm,info);

% solve using dfp_min routine
res2 = dfp_min('banana',parm,info);

% solve using pow_min routine
res3 = pow_min('banana',parm,info);

% solve using maxlik routine
info2.method = 'bfgs'; 
res4 = maxlik('banana',parm,info);

in.cnames = strvcat('frpr','dfp','pow','maxlik');
in.rnames = strvcat('parameters','b1','b2');
in.fmt = strvcat('%10.7f');
mprint([res1.b res2.b res3.b res4.b],in);

in.rnames = strvcat('function values','f');
mprint([res1.f res2.f res3.f res4.f],in);

in.rnames = strvcat('time taken','seconds');
mprint([res1.time res2.time res3.time res4.time],in);

in.fmt = strvcat('%8d');
in.rnames = strvcat('iterations','# iters');
mprint([res1.iter res2.iter res3.iter res4.iter],in);



