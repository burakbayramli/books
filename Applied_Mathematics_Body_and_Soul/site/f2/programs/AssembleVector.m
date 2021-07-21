function b = AssembleVector(points, edges, triangles, pde, W, time)

% Assemble a vector on a given mesh (points, edges, triangles)
% from a given variational formulation (pde).
%
% Copyright (C) 2003 Johan Hoffman and Anders Logg.
% Licensed under the GNU GPL Version 2.

% Create matrix
b = zeros(size(points,2),1);

% Quadrature points
midpoints = [0.5 0.0; 0.5 0.5; 0.0 0.5]';
gausspoints = [(1-1/sqrt(3))/2 (1-1/sqrt(3))/2];

% Iterate over all elements (interior domain)
for triangle = triangles

  % Get nodes, coordinates, and domain number
  nodes = triangle(1:3);
  coord = points(:,nodes);
  d = triangle(4);

  % Compute Jacobian of map and area of element
  J = coord(:,1)*dphi(1)' + coord(:,2)*dphi(2)' + coord(:,3)*dphi(3)';
  dx = 0.5 * abs(det(J));
  
  % Assemble matrix
  for p = midpoints

    x = coord(:,1)*phi(1,p) + coord(:,2)*phi(2,p) + coord(:,3)*phi(3,p); 

    if ~isempty(W)
      w  = evalfunctions(W(nodes,:), p);
      dw = evalderivatives(W(nodes,:), p);
    else
      w  = [];
      dw = [];
    end
    
    for i = 1:3
      
      v = phi(i,p);
      dv = J' \ dphi(i);
      
      integral = feval(pde, 0, v, w, [0;0], dv, dw, dx, 0, x, d, time, 2) / 3.0;
      b(nodes(i)) = b(nodes(i)) + integral;
	
    end
  end

end

% Iterate over all edges (boundary)
for edge = edges

  % Get nodes, coordinates, and domain number
  nodes = edge(1:2);
  coord = points(:,nodes);
  d = edge(5);

  % Compute length of edge
  ds = norm(coord(:,1) - coord(:,2));

  % Assemble matrix
  for p = gausspoints

    x = coord(:,1)*phiedge(1,p) + coord(:,2)*phiedge(2,p);

    if ~isempty(W)
      w  = evalfunctionsedge(W(nodes,:), p);
      dw = evalderivativesedge(W(nodes,:), p);
    else
      w  = [];
      dw = [];
    end
    
    for i = 1:2

      v = phiedge(i,p);
      
      integral = feval(pde, 0, v, w, [0;0], [0;0], dw, 0, ds, x, d, time, 2) / 2.0;
      b(nodes(i)) = b(nodes(i)) + integral;

    end
  end

end

%--- Basis functions on the reference element ---
function y = phi(index, x)

switch index
  case 1
    y = 1 - x(1) - x(2);
  case 2
    y = x(1);
  case 3
    y = x(2);
end

%--- Basis functions on the reference edge ---
function y = phiedge(index, x)

switch index
  case 1
    y = 1 - x;
  case 2
    y = x;
end
  
%--- Gradients of basis functions on the reference element ---
function y = dphi(index)

switch index
  case 1
    y = [-1; -1];
  case 2
    y = [1; 0];
  case 3
    y = [0; 1];
end

%--- Gradients of basis functions on the reference edge ---
function y = dphiedge(index)

switch index
  case 1
    y = -1;
  case 2
    y = 1;
end

%--- Evaluate functions at the given quadrature point ---
function w = evalfunctions(W, p)

n = size(W,2);
w = zeros(1,n);

for i = 1:n
 w(i) = W(1,i)*phi(1,p) + W(2,i)*phi(2,p) + W(3,i)*phi(3,p);
end

%--- Evaluate derivatives of functions at the given quadrature point ---
function dw = evalderivatives(W, p)

n = size(W,2);
dw = zeros(2,n);

for i = 1:n
  dw(:,i) = W(1,i)*dphi(1) + W(2,i)*dphi(2) + W(3,i)*dphi(3);	
end

%--- Evaluate functions at the given quadrature point ---
function w = evalfunctionsedge(W, p)

n = size(W,2);
w = zeros(1,n);

for i = 1:n
 w(i) = W(1,i)*phiedge(1,p) + W(2,i)*phiedge(2,p);
end

%--- Evaluate derivatives of functions at the given quadrature point ---
function dw = evalderivativesedge(W, p)

n = size(W,2);
dw = zeros(2,n);

for i = 1:n
  dw(1,i) = W(1,i)*dphiedge(1) + W(2,i)*dphiedge(2);
end
