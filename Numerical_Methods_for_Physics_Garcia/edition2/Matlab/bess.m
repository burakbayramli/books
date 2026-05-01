function jj = bess(m_max,x)
% Bessel function
% Inputs
%    m_max = Largest desired order
%    x = Value at which Bessel function J(x) is evaluated
% Output
%    jj = Vector of J(x) for all orders <= m_max

%* Perform downward recursion from initial guess
m_top = max(m_max,x)+15;    % Top value of m for recursion
m_top = 2*ceil( m_top/2 );  % Round up to an even number
j(m_top+1) = 0;
j(m_top) = 1;
for m=m_top-2:-1:0         % Downward recursion
  j(m+1) = 2*(m+1)/(x+eps)*j(m+2) - j(m+3);
end

%* Normalize using identity and return requested values
norm = j(1);               % NOTE: Be careful, m=0,1,... but
for m=2:2:m_top            % vector goes j(1),j(2),...
  norm = norm + 2*j(m+1);
end
for m=0:m_max              % Send back only the values for
  jj(m+1) = j(m+1)/norm;   % m=0,...,m_max and discard values
end                        % for m=m_max+1,...,m_top
