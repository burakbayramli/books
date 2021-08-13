function [gauss]=shape_functions_Gauss_points_2D_v(gauss)

% Computation of shape functions (and derivatives) at Gauss points for
% velocity
n_gauss=length(gauss);
for n=1:n_gauss
    gauss(n).N_v=f_N_2D_v(gauss(n).csi,gauss(n).eta);
    gauss(n).dN_v=f_dN_2D_v(gauss(n).csi,gauss(n).eta);
end

end