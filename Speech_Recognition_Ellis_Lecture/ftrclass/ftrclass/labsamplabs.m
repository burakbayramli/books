function Y = labsamplabs(X,T,L)
% Y = labsamplabs(X,T,L)   Sample label definitions at defined time points
%       X defines a set of sample times (for instance, the center times of 
%       a set of feature frames.  Y is returned as the value of the label 
%       definition at each time, drawn from the time ranges defined in 
%       T and the corresponding label indices in L, as returned by 
%       labreadlab.
% 2001-03-28 dpwe@ee.columbia.edu

nlabs = size(T,1);
ntimes = length(X);
symlen = size(L,2);

if size(X,1) > 1
  X = X';
end

%(X'*ones(1,nlabs))<(ones(ntimes,1)*T(:,2)')

ix = sum(((X'*ones(1,nlabs))>=(ones(ntimes,1)*T(:,1)'))');

Lx = [0;L;0];

Y = double(Lx(ix+1)');

