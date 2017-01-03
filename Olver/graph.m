function A = graph(B,V,rad,fr)
%
%   A = graph(B,V,rad,fr)
%    
%      Computes incidence matrix and plots directed graph
%
%  B - 2 column matrix giving the edges
%        the ith row of B says which two nodes are connected by edge i
%  V - 2 column matrix giving (x,y) coordinates of nodes to be used
%         in a figure. If omitted, no figure is drawn.
%  rad - gives size of circles at nodes and triangle at grounded node
%        default is .08
%  fr - gives fraction of distance along edge to write amount of current
%        default fraction is 4/7
%
%  A - incidence matrix for graph
%
%   See also CEDGE, INCIDENCE, NETW, CGON, NGON

A = incidence(B); 
nedge = size(A,1);
nvert = size(A,2);

if nargin < 3, rad = .08; end
if nargin < 4, fr = 4/7; end

  nedge = size(B,1);
  nvert = size(V,1);
  d = size(V,2);

  clf;

  mm = [min(V) - .5; max(V) + .5];
  axis(mm(:)); axis equal;

  for i=1:nedge,
    k = B(i,1); l = B(i,2);
    x1 = V(k,:); x2 = V(l,:);
    arrowvec(x1,x2,'b');
    xm = (1-fr)*x1 + fr*x2;
    textvec(xm,i);
  end

  for m=1:nvert
    x1 = V(m,:); 
    circle(x1,rad);
    textvec(x1,m,'r');
  end
