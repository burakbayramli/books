function B = cedge(n)
%
%  B = cedge(n)
%    
%  Determines edge matrix for complete graph through n vertices
%
%  B will be a 2 by n(n-1)/2 matrix giving the edges
%        the ith row of B says which two nodes are connected by edge i
%
%   See also CGRAPH, CGON, GRAPH, INCIDENCE, NETW, NGON

k = 1;

for i = 1:n-1,
  for j = i+1:n,
     B(k,1) = i;
	 B(k,2) = j;
	 k = k+1;
  end
end

