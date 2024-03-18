function f2 = prlng(f)
% prolongation of W with nozzle code
% cell arrangement
% aug 2017
n  = size(f,1); % 6
nc = size(f,2);
i  = 2:n-1 ;    % 2 3 4 5
i2 = 2:2:2*n-4; % 2 4 6 8
n2 = 2*n-3 ;    % 9
f2 = zeros(n2,nc);
f2(i2,:)    = f(i,:);
% 3 5 7                 2 4 6          4 6 8
f2(3:2:n2-2,:) = 0.5*(f2(2:2:n2-3,:)+f2(4:2:n2-1,:));
f2(1,:) = (f(1,:)+f(2,:))/2;
f2(n2,:)= (f(n,:)+f(n-1,:))/2;