function  S = chebsamp(range, q, gam)
%CHEBSAMP  n-dimensional Chebyshev grid over given range
%
% Call:    S = chebsamp(range, q, gam)
%
% range :  2*n matrix with lower and upper limits
% q     :  n-vector, q(j) is the number of points
%          in the j'th direction.
%          If q is a scalar, then all q(j) = q
% gam   :  n-vector, gam(j) is the density parameter in the jth direction. 
%          The distribution of nodes has density proportional to
%          (1-x^2)^(-gam). (see Fornberg book, and fdnodes.m)
% S     :  m*n array with points, m = prod(q)

% hbn@imm.dtu.dk  
% Last update June 25, 2002
% modified for generalized Chebyshev by Greg Fasshauer, June 9, 2008

[mr n] = size(range);    dr = diff(range);
if  mr ~= 2 | any(dr < 0)
  error('range must be an array with two rows and range(1,:) <= range(2,:)')
end 
sq = size(q);
if  min(sq) > 1 | any(q <= 0)
  error('q must be a vector with non-negative elements')
end
p = length(q);   
if  p == 1,  q = repmat(q,1,n); 
elseif  p ~= n
  error(sprintf('length of q must be either 1 or %d',n))
end 
p = length(gam);   
if  p == 1,  gam = repmat(gam,1,n); 
elseif  p ~= n
  error(sprintf('length of gam must be either 1 or %d',n))
end 

% Check for degenerate intervals
i = find(dr == 0);
if  ~isempty(i),  q(i) = 0*q(i); end

% Recursive computation
if  n > 1
  A = chebsamp(range(:,2:end), q(2:end), gam(2:end));  % Recursive call
  [m p] = size(A);   q = q(1);
  S = [zeros(m*q,1) repmat(A,q,1)];
  y = fdnodes(gam(1),q-1,[range(1,1) range(2,1)]).';
  k = 1:m;
  for  i = 1 : q
    S(k,1) = repmat(y(i),m,1);  k = k + m;
  end
else    
  S = fdnodes(gam(1),q-1,[range(1,1) range(2,1)]);
end