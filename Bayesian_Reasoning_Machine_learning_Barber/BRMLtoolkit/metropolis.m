function xnew=metropolis(x,s,logp)
%METROPOLIS Metropolis sample
% xnew=metropolis(x,s,logp)
% Get a new sample xnew from distribution exp(logp) using
% metropolis sampling with standard deviation s, and current sample x
% See also demoMetropolis.m
xcand=x+s*randn(size(x));
loga=feval(logp,xcand)-feval(logp,x);
if loga>0
	xnew=xcand;
else
	r=rand;
	if log(r)<loga
		xnew=xcand;
	else
		xnew=x;
	end
end