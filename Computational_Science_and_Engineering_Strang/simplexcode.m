%8.6  simplexcode.m

function [x,y,cost] = simplex(A,b,c,basis)
  disp(basis);
  B=A(:,basis); x(basis) = B\b;  % xpos at starting corner
  if any (x(basis) < -.000001)
    error('Starting x has a component < 0')
  end
  cost = c(basis)'*x(basis);     % cost at starting corner
  for iter = 1:100
    y = B'\c(basis);           % this y may not be feasible
    [rmin,in] = min(c - A'*y); % minimum r and its index in
    if rmin >= -.00000001      % optimality is reached, r>=0
        break;                 % current x and y are optimal
    end

    v(basis) = B\A(:,in);  % decrease in x from 1 unit of xin
    [minratio,out] = min(x(basis)./max(v(basis),.000001));
    if v(out) == .000001  % out = index of first x to reach 0
        break;      % break when that edge is extremely short
    end

    cost = cost + minratio*rmin;  % lower cost at end of step
    x(basis) = x(basis) - minratio*v(basis);   % update old x
    x(in) = minratio;      % find new positive component of x
    basis(out) = in;      % replace old index by new in basis
  end

