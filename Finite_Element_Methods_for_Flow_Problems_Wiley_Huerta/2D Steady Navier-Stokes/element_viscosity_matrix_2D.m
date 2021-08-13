function [K]=element_viscosity_matrix_2D(v,dof_el_v,gauss,J)

% Element viscosity matrix
n_gauss=length(gauss);
n_sd=2;
K=zeros(dof_el_v*n_sd,dof_el_v*n_sd);
for a=1:dof_el_v
    for b=1:dof_el_v
        for i=1:n_sd
            for j=1:n_sd
                r=n_sd*(a-1)+i;
                s=n_sd*(b-1)+j;
                for n=1:n_gauss
                    BT_B=gauss(n).B(a).B'*gauss(n).B(b).B;
                    K(r,s)=K(r,s)+BT_B(i,j)*gauss(n).w;
                end
                K(r,s)=v*K(r,s)*J;
            end
        end
    end
end

end