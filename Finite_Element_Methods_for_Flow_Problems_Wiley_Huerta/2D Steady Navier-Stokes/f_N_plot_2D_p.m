function [N] = f_N_plot_2D_p(csi,eta)

% Shape functions to plot for pressure
for i=1:length(csi)
    for j=1:length(eta)
        N(1).N(i,j)=1/4*(1-csi(i)).*(1-eta(j));
        N(2).N(i,j)=1/4*(1+csi(i)).*(1-eta(j));
        N(3).N(i,j)=1/4*(1+csi(i)).*(1+eta(j));
        N(4).N(i,j)=1/4*(1-csi(i)).*(1+eta(j));
    end
end

end