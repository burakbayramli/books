function [C]=assemble_convection_matrix(el,dof_v,n_el,dof_el_v,A_v)

% Assemblage of convection matrix
n_sd=2;
C=zeros(dof_v*n_sd,dof_v*n_sd);
for n=1:n_el
    for a=1:dof_el_v
        for b=1:dof_el_v
            for i=1:n_sd
                for j=1:n_sd
                    r=n_sd*(a-1)+i;
                    s=n_sd*(b-1)+j;
                    C(A_v(n,r),A_v(n,s))=C(A_v(n,r),A_v(n,s))+el(n).C(r,s);
                end
            end
        end
    end 
end

end