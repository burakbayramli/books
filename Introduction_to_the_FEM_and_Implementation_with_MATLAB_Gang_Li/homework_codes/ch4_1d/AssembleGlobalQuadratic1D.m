% Assemble element stiffness matrix k and force vector f into the 
% global matrix K and vector F
% Input: K, F: (empty) global stiffness matrix and force vector
% Input: eid: elemend index in the mesh
% Input: elements: the element matrix loaded from "elements.dat"
% Input: k,f: local stiffness matrix and force vector
% Output: K, F: Assembled global stiffness matrix and force vector
function [K,F]=AssembleGlobalQuadratic1D(K, F, eid, elements, k, f)
gids=[elements(eid,2:4)]';               % global indices of the 
                                         % nodes of element eid
for m=1:3                                % loop over rows                  
    F(gids(m))=F(gids(m))+f(m);          % f(m) added onto global F
    for n=1:3                            % loop over columns                      
      K(gids(m),gids(n))=K(gids(m),gids(n))+k(m,n); % k(m,n) added                    
    end                                             % onto K
  end