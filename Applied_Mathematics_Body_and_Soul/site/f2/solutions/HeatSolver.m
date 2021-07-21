% Load the mesh
square
%square_refined

T = pi/2;
k = T/10;
time = 0;

% Initial condition
U0 = zeros(size(p,2),1);
U1 = zeros(size(p,2),1);

% Assemble matrix
A = AssembleMatrix(p, e, t, 'Heat', [], 0);

% Time-stepping
for n = 1:10
   
  time = time + k;
  U0 = U1;

  % Assemble vector
  b = AssembleVector(p, e, t, 'Heat', U0, time);

  % Solve the linear system
  U1 = A \ b;
   
end

% Compute exact solution
u = zeros(size(U1));
for i = 1:size(p,2)
  x = p(:,i);
  u(i) = x(1)*(1-x(1))*x(2)*(1-x(2))*sin(T);
end

% Compute the error
error = U1 - u;
enorm = max(abs(error));
disp(['Maximum norm error: ' num2str(enorm)])

% Plot solution, exact solution, and error
figure(1); clf
pdesurf(p,t,U1)
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

