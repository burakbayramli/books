function [x,y,v,A,K] = network(B,R,f,V,n,rad,fr)
%
%   [x,y,v,A,K] = network(B,R,f,V,n,fr)
%    
%      Computes the currents through a simple electrical network
%
%  B - 2 column matrix giving the edges
%        the ith row of B says which two nodes are connected by edge i
%  R - vector of resistances on edges
%       if  R is a scalar then all resistances are equal
%  f - vector of current sources at nongrounded nodes
%       if f = i then use a unit current source at node #i
%  V - 2 column matrix giving (x,y) coordinates of nodes to be used
%         in a figure. If omitted, no figure is drawn.
%  n - node to be grounded
%       default is last node
%  rad - gives size of circles at nodes and triangle at grounded node
%        default is .08
%  fr - gives fraction of distance along edge to write amount of current
%        default fraction is 4/7
%
%  x - potentials at nongrounded nodes
%  y - currents on edges
%  v - voltages on adges
%  A - incidence matrix for graph
%  K = A'*C*A - "stiffness" matrix
%  C = diag(1./R) - conductivity matrix
%
%   See also CEDGE, INCIDENCE, GRAPH, CGON, NGON

A = incidence(B); 
nedge = size(A,1);
nvert = size(A,2);

if nargin < 5, n = nvert; end

A(:,n) = [];

if max(size(R)) == 1,
   C = eye(nedge)/R ;
  else
   C = diag(1./R);
end

if max(size(f)) == 1,
   F = zeros(nvert-1,1); F(f) = 1;
  else
   F = f;
end

K = A' * C * A;
x = K\F;
v = A * x;
y = C * v;

if nargin > 3, 

if nargin < 6, rad = .08; end
if nargin < 7, fr = 4/7; end

  nedge = size(B,1);
  nvert = size(V,1);

  clf;

  mm = [min(V) - .5; max(V) + .5];
  axis(mm(:)); axis equal;

  for i=1:nedge,
    k = B(i,1); l = B(i,2);
    x1 = V(k,:); x2 = V(l,:);
    arrowvec(x1,x2,'b');
    xm = (1-fr)*x1 + fr*x2;
    textvec(xm,y(i));
  end

  for m=1:nvert
    x1 = V(m,:); 
   if m == n
    triangle(x1,rad,'c');
    textvec(x1,0,'r'); 
   else
    circle(x1,rad);
    if m < n, 
	   textvec(x1,x(m),'r');
	else
	   textvec(x1,x(m-1),'r'); 
    end
   end
  end

end
