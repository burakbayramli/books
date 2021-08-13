function [G]=element_gradient_operator_matrix_2D(dof_el_v,dof_el_p,gauss,J,L_el_x,L_el_y)

% Element gradient operator matrix
L_el=[L_el_x,L_el_y];
n_gauss=length(gauss);
n_sd=2;
G=zeros(dof_el_v*n_sd,dof_el_p);
for a=1:dof_el_v
    for i=1:n_sd
        r=n_sd*(a-1)+i;
        for k=1:dof_el_p
            for n=1:n_gauss
                G(r,k)=G(r,k)+(gauss(n).N_p(k).N*gauss(n).dN_v(a).dN(i))*gauss(n).w;
            end
            G(r,k)=-G(r,k)*2/L_el(i)*J;
        end
    end
end

end