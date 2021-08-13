function []=plot_test_functions_2D(csi,eta,W,dof_el)

% Plot of test functions
[CSI,ETA]=meshgrid(csi,eta);
figure('Color',[1 1 1])
for n=1:dof_el
    subplot(ceil(sqrt(dof_el)),ceil(sqrt(dof_el)),n,'FontSize',14)
    surf(ETA,CSI,W(n).W)
    title(['Test function n. ',num2str(n)],'FontSize',14)
    xlabel('\xi','FontSize',14)
    ylabel('\eta','FontSize',14)
    zlabel('W(\xi,\eta)','FontSize',14)
    grid on
    grid minor
    xlim([-1.1,+1.1])
    ylim([-1.1,+1.1])
    zlim([-0.5,+1.5])
end

end

