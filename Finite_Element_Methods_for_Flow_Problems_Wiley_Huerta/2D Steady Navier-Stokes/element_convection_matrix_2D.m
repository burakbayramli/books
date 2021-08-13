function [C]=element_convection_matrix_2D(u_el,dof_el_v,gauss,J,L_el_x,L_el_y)

% Element convection matrix
L_el=[L_el_x,L_el_y];
n_gauss=length(gauss);
n_sd=2;
C=zeros(dof_el_v*n_sd,dof_el_v*n_sd);
for a=1:dof_el_v
    for b=1:dof_el_v
        for i=1:n_sd
            for j=1:n_sd
                r=n_sd*(a-1)+i;
                s=n_sd*(b-1)+j;
                for n=1:n_gauss
                    Matrix=gauss(n).N_v(a).N*u_el(n_sd*(a-1)+1:n_sd*(a-1)+2,1)*(gauss(n).dN_v(b).dN'.*(2./L_el));
                    C(r,s)=C(r,s)+Matrix(i,j)*gauss(n).w;
                end
                C(r,s)=C(r,s)*J;
            end
        end
    end
end

end