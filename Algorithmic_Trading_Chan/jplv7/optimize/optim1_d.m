% PURPOSE: An example using dfp_min, frpr_min, pow_min, maxlik
%                           
%  to solve a Tobit (censored) regression maximum
%  likelihood problem                           
%---------------------------------------------------
% USAGE: optim1_d
%---------------------------------------------------

% generate uncensored data
n=200; k=8; randn('seed',20201); x = randn(n,k); beta = ones(k,1);
y = x*beta + randn(n,1);
% now censor the data
for i=1:n
 if y(i,1) < 0, y(i,1) = 0.0; end;
end;
% use ols for starting values
res = ols(y,x); b = res.beta; sige = res.sige;
 parm = [b  
        sige];  % starting values 
info.maxit = 1000;      
% solve using frpr_min routine
result1 = frpr_min('to_like1',parm,info,y,x);

% solve using dfp_min routine
result2 = dfp_min('to_like1',parm,info,y,x);

% solve using pow_min routine
result3 = pow_min('to_like1',parm,info,y,x);

% solve using maxlik routine
info2.method = 'bfgs'; 
result4 = maxlik('to_like1',parm,info2,y,x);

% formatting information for mprint routine
cnames = strvcat('fprf','dfp','powell','bfgs'); in.fmt = '%8.4f';
fprintf(1,'comparison of  estimates \n');

rnames = strvcat('parameters');
for i=1:k;
bstring = ['b' num2str(i)];
rnames = strvcat(rnames,bstring);
end;
rnames = strvcat(rnames,'sigma');
in.cnames = cnames;
in.rnames = rnames;
in.fmt = '%12.4f';

mprint([result1.b result2.b result3.b result4.b],in);
fprintf(1,'comparison of likelihood functions \n');
in2.cnames = cnames;
mprint([result1.f result2.f result3.f result4.f],in2);
in.fmt = '%4d'; fprintf(1,'comparison of # of iterations \n');
mprint([result1.iter result2.iter result3.iter result4.iter],in2);
fprintf(1,'comparison of hessians \n'); 
in3.fmt = '%8.2f';
fprintf(1,'fprf hessian');   mprint(result1.hess,in3);
fprintf(1,'dfp hessian');    mprint(result2.hess,in3);
fprintf(1,'powell hessian'); mprint(result3.hess,in3);
fprintf(1,'maxlik hessian'); mprint(result4.hess,in3);


