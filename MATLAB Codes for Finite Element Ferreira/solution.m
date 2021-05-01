function displacements=solution(GDof,prescribedDof,stiffness,force)
% function to find solution in terms of global displacements
% GDof: number of degree of freedom
% prescribedDof: bounded boundary dofs
% stiffness: stiffness matrix
% force: force vector
%%
activeDof = setdiff((1:GDof)', prescribedDof);
U = stiffness(activeDof,activeDof)\force(activeDof);
displacements = zeros(GDof,1);
displacements(activeDof) = U;
