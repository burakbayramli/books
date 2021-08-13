function [gauss]=strain_rate_velocity_matrix_2D(dof_el_v,gauss,L_el_x,L_el_y)

% Strain rate - velocity matrix
n_gauss=length(gauss);
for n=1:n_gauss
    for a=1:dof_el_v
        gauss(n).B(a).B=[gauss(n).dN_v(a).dN(1)*2/L_el_x             0
                         gauss(n).dN_v(a).dN(2)*2/L_el_y             0
                                   0              gauss(n).dN_v(a).dN(1)*2/L_el_x
                                   0              gauss(n).dN_v(a).dN(2)*2/L_el_y];
    end
end

end