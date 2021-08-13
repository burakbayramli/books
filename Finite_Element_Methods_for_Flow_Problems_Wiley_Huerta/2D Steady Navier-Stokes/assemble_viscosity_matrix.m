function [K]=assemble_viscosity_matrix(el,dof_v,n_el,dof_el_v,A_v)

% Assemblage of viscosity matrix
n_sd=2;
K=zeros(dof_v*n_sd,dof_v*n_sd);
for n=1:n_el
    for a=1:dof_el_v
        for b=1:dof_el_v
            for i=1:n_sd
                for j=1:n_sd
                    r=n_sd*(a-1)+i;
                    s=n_sd*(b-1)+j;
                    K(A_v(n,r),A_v(n,s))=K(A_v(n,r),A_v(n,s))+el(n).K(r,s);
                end
            end
        end
    end 
end

end