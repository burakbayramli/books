function A = incidence(B)
%
%  A = incidence(B)
%    
%  Determines incidence matrix A corresponding to edge matrix B
%
%  B is a matrix with 2 columns giving the edges
%        the ith row of B says which two nodes are connected by edge i
%
%   See also CEDGE, NETW

nedge = size(B,1);

nvert = max(max(B));

A = zeros(nedge,nvert);

for i=1:nedge,
  A(i,B(i,1)) = 1;
  A(i,B(i,2)) = -1;
end


