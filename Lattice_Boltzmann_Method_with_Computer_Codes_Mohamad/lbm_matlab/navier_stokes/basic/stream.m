function f = stream(f)
% Stream 2DQ9 on a 2d matrix.
f(:,2:end,2) = f(:,1:end-1,2); % East vector.
f(2:end,:,3) = f(1:end-1,:,3); % North vector.
f(:,1:end-1,4) = f(:,2:end,4); % West vector.
f(1:end-1,:,5) = f(2:end,:,5); % South vector.
f(2:end,2:end,6) = f(1:end-1,1:end-1,6); % Northeast vector.
f(2:end,1:end-1,7) = f(1:end-1,2:end,7); % Northwest vector.
f(1:end-1,1:end-1,8) = f(2:end,2:end,8); % Southwest vector.
f(1:end-1,2:end,9) = f(2:end,1:end-1,9); % Southeast vector.