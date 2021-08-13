function [gauss]=shape_functions_Gauss_points_2D_p(gauss)

% Computation of shape functions (and derivatives) at Gauss points for
% pressure
n_gauss=length(gauss);
for n=1:n_gauss
    gauss(n).N_p=f_N_2D_p(gauss(n).csi,gauss(n).eta);
    gauss(n).dN_p=f_dN_2D_p(gauss(n).csi,gauss(n).eta);
end

end