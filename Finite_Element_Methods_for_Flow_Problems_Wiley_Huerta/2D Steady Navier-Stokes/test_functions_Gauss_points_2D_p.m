function [gauss]=test_functions_Gauss_points_2D_p(gauss)

% Computation of test functions (and derivatives) at Gauss points for
% pressure
n_gauss=length(gauss);
for n=1:n_gauss
    gauss(n).W_p=f_W_2D_p(gauss(n).csi,gauss(n).eta);
    gauss(n).dW_p=f_dW_2D_p(gauss(n).csi,gauss(n).eta);
end

end