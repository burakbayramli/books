% Assemble element stiffness matrix 
% matrix K.
function K = assembly(K,e,ke)
include_flags;

for loop1 = 1:nen*ndof
    i = LM(loop1,e);
    for loop2 =  1:nen*ndof
        j = LM(loop2,e);
        K(i,j) = K(i,j) + ke(loop1,loop2);
    end
end

