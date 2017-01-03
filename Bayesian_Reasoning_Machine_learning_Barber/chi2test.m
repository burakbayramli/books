function xs = chi2test(k,sig)
%CHI2TEST inverse of the chi square cumulative density
% xs = chi2test(k,sig)
% k : degrees of freedom
% sig : significance
% returns significant point only in the range [0:0.01:1000]
x=[0:0.01:1000]';
for i=1:length(k)
    y= condexp(((k(i)/2)-1).*logeps(x) -x/2);
    ind=find(cumsum(y)>1-sig);
    xs(1,i)=x(ind(1));
end