function B = oedge(n)
%
%  B = oedge(n)
%    
%  Determines edge matrix for circular graph on n vertices
%
%  B will be a 2 by n matrix giving the edges
%        the ith row of B says which two nodes are connected by edge i
%
%   See also OGON, OGRAPH, CGRAPH, CGON, GRAPH, INCIDENCE, NETWORK, NGON

for k = 1:n-1,
     B(k,1) = k;
	 B(k,2) = k+1;
end

     B(n,1) = n;
	 B(n,2) = 1;

