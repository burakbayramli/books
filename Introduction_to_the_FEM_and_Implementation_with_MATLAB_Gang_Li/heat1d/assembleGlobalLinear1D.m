% Assemble element stiffness matrix k and force 
% vector f into the global matrix K and vector F
% the global node index of the two local nodes are
% gi and gj

function [K,F]=assembleGlobalLinear1D(eid, elements, k, f, K, F)

gids=[elements(eid,2) elements(eid,3)]';   
for m=1:2                                                  
    F(gids(m))=F(gids(m))+f(m);            
    for n=1:2                                                  
      K(gids(m),gids(n))=K(gids(m),gids(n))+k(m,n);                     
    end
  end
