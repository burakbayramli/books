function T2 = sumv(T1, sum_over)
% sumv(T, dims)  Sum multidimensional array T over dimensions 'dims' and squeeze the result
% This is like the built-in sum, but you can pass a vector of dimensions to sum over
% Example
% T = reshape(1:8, [2 2 2])
% sumv(T, [1 3]) = sum(sum(T,1),3) = [14 22] = [1+2 + 5+6, 3+4 + 7+8]
%  since
%T(:,:,1) =
%     1     3
%     2     4
%T(:,:,2) =
%     5     7
%     6     8

T2 = T1;
for i=1:length(sum_over)
  if sum_over(i) <= ndims(T2) % prevent summing over non-existent dimensions
    T2=sum(T2, sum_over(i));
  end
end
T2 = squeeze(T2);
