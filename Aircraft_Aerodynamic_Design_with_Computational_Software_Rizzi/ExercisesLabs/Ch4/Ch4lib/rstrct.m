function f2 = rstrct(f)
% restriction of solution and residual
% nozzle code arrangement of cells
% aug 2017
n  = size(f,1);
i  = 2:2:n;
f2 = [f(1,:);0.25*(f(i-1,:)+2*f(i,:)+f(i+1,:));f(end,:)];
%f2 = [f(1,:);f(i,:);f(end,:)];
