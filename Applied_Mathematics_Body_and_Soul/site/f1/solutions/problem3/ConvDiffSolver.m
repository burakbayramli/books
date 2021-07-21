% Load the mesh
cylinder

% Assemble matrix
A = AssembleMatrix(p, e, t, 'ConvDiff', [], 0);

% Assemble vector
b = AssembleVector(p, e, t, 'ConvDiff', [], 0);

% Solve the linear system
U = A \ b;

% Plot solution
figure(1); clf
pdesurf(p,t,U)
shading faceted
title('Computed solution')
