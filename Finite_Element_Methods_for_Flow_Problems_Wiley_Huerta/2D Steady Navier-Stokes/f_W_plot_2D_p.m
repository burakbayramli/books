function [W] = f_W_plot_2D_p(csi,eta)

% Test functions to plot for pressure
for i=1:length(csi)
    for j=1:length(eta)
        W(1).W(i,j)=1/4*(1-csi(i)).*(1-eta(j));
        W(2).W(i,j)=1/4*(1+csi(i)).*(1-eta(j));
        W(3).W(i,j)=1/4*(1+csi(i)).*(1+eta(j));
        W(4).W(i,j)=1/4*(1-csi(i)).*(1+eta(j));
    end
end

end