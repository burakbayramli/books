function [B,V] = ogon(n)
%
%  [B,V] = ogon(n)
%    
%  B - edge matrix for circular graph through n vertices
%    
%  V - coordinates of vectices on regular unit n-gon
%
%   See also OEDGE, OGRAPH, CEDGE, NGON, GRAPH, 

 
B = oedge(n);
V = ngon(n);
