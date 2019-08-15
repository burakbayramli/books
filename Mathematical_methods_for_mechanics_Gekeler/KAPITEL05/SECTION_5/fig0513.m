function fig0513
% Figure 5.13, Full brusselator

disp(' Call first DEMO2-4 ')
clf
load datenC4 YY OMGA MU Parmeter
YY = [YY, YY(:,1)];
YY0 = Parmeter(2:4);
YY0 = YY0*ones(1,size(YY,2));
K = size(YY,1);
for I = 1:K/3
   I = I-1;
   Z = YY(3*I+[1:3],:) + YY0;
   plot3(Z(1,:),Z(2,:),Z(3,:),'k','linewidth',2), hold on
end
xlabel('y_1','fontsize',24)
ylabel('y_2','fontsize',24)
zlabel('y_3','fontsize',24)
grid on
axis equal tight
