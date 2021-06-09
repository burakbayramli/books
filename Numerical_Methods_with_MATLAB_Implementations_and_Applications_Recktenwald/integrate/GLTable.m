function [x,w] = GLTable(n)
% GLTable  Nodes and weights for Gauss-Legendre quadrature of order n<=8
%
% Synopsis:  [x,w] = GLTable(n)
%
% Input:     n = number of nodes in quadrature rule, maximum: n = 8
%
% Output:    x = vector of nodes
%            w = vector of weights

% Numerical values from "Handbook of Mathematical Functions",
% Abramowitz and Stegun, eds., 1965 Dover (reprint), Table 25.4, p. 916

nn = fix(n);                %  Make sure number of nodes is an integer
x = zeros(nn,1);  w = x;    %  Preallocate x and w vectors

switch nn
  case 1
    x = 0;   w = 2;

  case 2
    x(1) = -1/sqrt(3);     x(2) = -x(1);
    w(1) =  1;             w(2) =  w(1);

  case 3
    x(1) = -sqrt(3/5);    x(2) =  0;      x(3) = -x(1);
    w(1) =  5/9;          w(2) =  8/9;    w(3) =  w(1);

  case 4
    x(1) = -0.861136311594053;     x(4) = -x(1);
    x(2) = -0.339981043584856;     x(3) = -x(2);
    w(1) =  0.347854845137454;     w(4) =  w(1);
    w(2) =  0.652145154862546;     w(3) =  w(2);

  case 5
    x(1) = -0.906179845938664;     x(5) = -x(1);
    x(2) = -0.538469310105683;     x(4) = -x(2);
    x(3) =  0;
    w(1) =  0.236926885056189;     w(5) =  w(1);
    w(2) =  0.478628670499366;     w(4) =  w(2);
    w(3) =  0.568888888888889;

  case 6
    x(1) = -0.932469514203152;     x(6) = -x(1);
    x(2) = -0.661209386466265;     x(5) = -x(2);
    x(3) = -0.238619186083197;     x(4) = -x(3);
    w(1) =  0.171324492379170;     w(6) =  w(1);
    w(2) =  0.360761573048139;     w(5) =  w(2);
    w(3) =  0.467913934572691;     w(4) =  w(3);

  case 7
    x(1) = -0.949107912342759;     x(7) = -x(1);
    x(2) = -0.741531185599394;     x(6) = -x(2);
    x(3) = -0.405845151377397;     x(5) = -x(3);
    x(4) =  0;
    w(1) =  0.129484966168870;     w(7) =  w(1);
    w(2) =  0.279705391489277;     w(6) =  w(2);
    w(3) =  0.381830050505119;     w(5) =  w(3);
    w(4) =  0.417959183673469;

  case 8
    x(1) = -0.960289856497536;     x(8) = -x(1);
    x(2) = -0.796666477413627;     x(7) = -x(2);
    x(3) = -0.525532409916329;     x(6) = -x(3);
    x(4) = -0.183434642495650;     x(5) = -x(4);
    w(1) =  0.101228536290376;     w(8) =  w(1);
    w(2) =  0.222381034453374;     w(7) =  w(2);
    w(3) =  0.313706645877887;     w(6) =  w(3);
    w(4) =  0.362683783378362;     w(5) =  w(4);

  otherwise
    error(sprintf('Gauss quadrature with %d nodes not supported',nn));

end
