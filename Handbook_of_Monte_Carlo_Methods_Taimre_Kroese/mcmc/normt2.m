function out=normt2(mu,sig,a,b)
% draws from the conditional pdf
% of phi((x-mu)/sigma)/sigma  on the
% the set {{x<a} U {x>b}},
% where phi(x) is the standard normal.
% Uses inverse method.
pb=normcdf((b-mu)/sig);
pa=normcdf((a-mu)/sig);
if rand<pa/(pa+1-pb)
      out=mu+sig*norminv(pa*rand(size(mu)));
else
      out=mu+sig*norminv((1-pb)*rand(size(mu))+pb);
end

