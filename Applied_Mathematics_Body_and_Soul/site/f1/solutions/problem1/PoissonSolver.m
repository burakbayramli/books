% Load the mesh
square
%square_refined

% Assemble matrix
A = AssembleMatrix(p, e, t, 'Poisson', [], 0);

% Assemble vector
b = AssembleVector(p, e, t, 'Poisson', [], 0);

% Solve the linear system
U = A \ b;

% Compute exact solution
u = zeros(size(U));
for i = 1:size(p,2)
  x = p(:,i);
  u(i) = sin(pi*x(1)) * sin(2*pi*x(2));
end

% Compute the error
error = U - u;
enorm = max(abs(error));
disp(['Maximum norm error: ' num2str(enorm)])

% Plot solution, exact solution, and error
figure(1); clf
pdesurf(p,t,U)
shading faceted
title('Computed solution')

figure(2); clf
pdesurf(p,t,u);
shading faceted
title('Exact solution')

figure(3); clf
pdesurf(p,t,error)
shading faceted
title('Error')
