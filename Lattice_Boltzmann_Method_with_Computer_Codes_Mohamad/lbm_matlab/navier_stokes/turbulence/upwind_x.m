function d = upwind_x(s, u, dh)
% takes upwind difference
% indexing: s(j,i)

d = 0*s;

% boundaries.
d(:,1) = ( s(:,2) - s(:,1) ) / dh;
d(:,end) = ( s(:,end) - s(:,end-1) ) / dh;
% interior.
mask = u(:,2:end-1) >= 0;
d(:,2:end-1) = ...
    (1-mask) .* s(:, (2:end-1)+1 ) ...
    + (-1+2*mask) .* s(:, 2:end-1 ) ...
    - mask .* s(:, (2:end-1)-1 );


