function [G]=assemble_gradient_operator_matrix(el,dof_v,dof_p,n_el,dof_el_v,dof_el_p,A_v,A_p)

% Assemblage of gradient operator matrix
n_sd=2;
G=zeros(dof_v*n_sd,dof_p);
for n=1:n_el
    for a=1:dof_el_v
        for i=1:n_sd
            r=n_sd*(a-1)+i;
            for k=1:dof_el_p
                G(A_v(n,r),A_p(n,k))=G(A_v(n,r),A_p(n,k))+el(n).G(r,k);
            end
        end
    end
end

end