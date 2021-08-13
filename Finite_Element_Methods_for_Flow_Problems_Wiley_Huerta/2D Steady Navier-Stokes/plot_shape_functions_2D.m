function []=plot_shape_functions_2D(csi,eta,N,dof_el)

% Plot of shape functions
[CSI,ETA]=meshgrid(csi,eta);
figure('Color',[1 1 1])
for n=1:dof_el
    subplot(ceil(sqrt(dof_el)),ceil(sqrt(dof_el)),n,'FontSize',14)
    surf(ETA,CSI,N(n).N)
    title(['Shape function n. ',num2str(n)],'FontSize',14)
    xlabel('\xi','FontSize',14)
    ylabel('\eta','FontSize',14)
    zlabel('N(\xi,\eta)','FontSize',14)
    grid on
    grid minor
    xlim([-1.1,+1.1])
    ylim([-1.1,+1.1])
    zlim([-0.5,+1.5])
end

end