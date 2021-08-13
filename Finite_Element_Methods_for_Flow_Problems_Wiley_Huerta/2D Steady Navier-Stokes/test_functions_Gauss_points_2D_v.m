function [gauss]=test_functions_Gauss_points_2D_v(gauss)

% Computation of test functions (and derivatives) at Gauss points for
% velocity
n_gauss=length(gauss);
for n=1:n_gauss
    gauss(n).W_v=f_W_2D_v(gauss(n).csi,gauss(n).eta);
    gauss(n).dW_v=f_dW_2D_v(gauss(n).csi,gauss(n).eta);
end

end