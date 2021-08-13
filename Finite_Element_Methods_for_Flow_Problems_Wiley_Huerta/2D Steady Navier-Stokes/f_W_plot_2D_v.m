function [W] = f_W_plot_2D_v(csi,eta)

% Test functions to plot for velocity
for i=1:length(csi)
    for j=1:length(eta)
        W(1).W(i,j)=+1/4*(1-csi(i))*(1-eta(j))*csi(i)*eta(j);
        W(2).W(i,j)=-1/4*(1+csi(i))*(1-eta(j))*csi(i)*eta(j);
        W(3).W(i,j)=+1/4*(1+csi(i))*(1+eta(j))*csi(i)*eta(j);
        W(4).W(i,j)=-1/4*(1-csi(i))*(1+eta(j))*csi(i)*eta(j);
        W(5).W(i,j)=-1/2*(1-eta(j))*eta(j)*(1-csi(i)^2);
        W(6).W(i,j)=+1/2*(1+csi(i))*csi(i)*(1-eta(j)^2);
        W(7).W(i,j)=+1/2*(1+eta(j))*eta(j)*(1-csi(i)^2);
        W(8).W(i,j)=-1/2*(1-csi(i))*csi(i)*(1-eta(j)^2);
        W(9).W(i,j)=+1*(1-csi(i)^2)*(1-eta(j)^2);
    end
end

end