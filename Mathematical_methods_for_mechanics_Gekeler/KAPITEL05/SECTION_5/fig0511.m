function fig0511
% Figure 5.11,  Feedback Inhibition Modell

clf
disp(' Call first DEMO2-2 ')
load datenC2 YY OMGA MU
%MUE
YY = [YY, YY(:,1)];
K = size(YY,1);
for I = 1:K/3
   I = I-1;
   plot3(YY(3*I+1,:),YY(3*I+2,:),YY(3*I+3,:),'k','linewidth',2), hold on
end
xlabel('y_1','fontsize',24)
ylabel('y_2','fontsize',24)
zlabel('y_3','fontsize',24)
grid on
axis equal tight
