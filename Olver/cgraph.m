function [A,B,V] = cgraph(n,rad,fr)
%
%   A = cgraph(B,V,rad,fr)
%    
%  Computes incidence matrix and plots complete directed graph
%        for n vertices
%  rad - gives size of circles at nodes and triangle at grounded node
%        default is .08
%  fr - gives fraction of distance along edge to write amount of current
%        default fraction is 4/7
%
%  A - incidence matrix for graph
%  B - 2 column matrix giving the edges
%        the ith row of B says which two nodes are connected by edge i
%  V - 2 column matrix giving (x,y) coordinates of nodes to be used
%         in a figure. If omitted, no figure is drawn.
%
%   See also CEDGE, CGON, NGON, OGRAPH, GRAPH, INCIDENCE, NETWORK, 

[B,V] = cgon(n);

if nargin < 2, rad = .08; end
if nargin < 3, fr = 4/7; end

A = graph(B,V,rad,fr);
