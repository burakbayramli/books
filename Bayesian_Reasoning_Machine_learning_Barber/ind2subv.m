function out = ind2subv(siz,ndx)
% out = ind2subv(siz,ndx)
%
% index to state : return a state vector of the linear index ndx, based on an array of size siz
% For vector ndx, returns a matrix with each row containing the corresponding state vector
k = [1 cumprod(siz(1:end-1))];
for i = length(siz):-1:1; vi = rem(ndx-1, k(i)) + 1; vj = (ndx - vi)./k(i) + 1;out(:,i) = vj;ndx = vi; end
