function[]=FEMresultplot(input,mesh)


Fi=input.Fi;
Mi=input.Mi;
Total=input.Total;
def=input.def;
R_el=input.R_el;
LF=input.Loads;

%% Plotting results
%% Computing indices
[a b]=size(def);
dx_index=1:6:(a-5);
dy_index=2:6:(a-4);
dz_index=3:6:(a-3);
dl_index=4:6:(a-2);
dm_index=5:6:(a-1);
dn_index=6:6:(a-0);



x_pos=[input.x];



figure(1)

subplot(3,2,1);
plot(x_pos,def(dx_index))
ylabel('\Delta x, [m]')
xlabel('Beam station, L, [m]')
title('Deformation in the global coordinate system')

subplot(3,2,3);
plot(x_pos,def(dy_index))
ylabel('\Delta y, [m]')
xlabel('Beam station, L, [m]')

subplot(3,2,5);
plot(x_pos,def(dz_index))
ylabel('\Delta z, [m]')
xlabel('Beam station, L, [m]')

subplot(3,2,2);
plot(x_pos,def(dl_index)*180/pi)
ylabel('\Delta\theta_x, [deg]')
xlabel('Beam station, L, [m]')

subplot(3,2,4);
plot(x_pos,def(dm_index)*180/pi)
ylabel('\Delta\theta_y, [deg]')
xlabel('Beam station, L, [m]')

subplot(3,2,6);
plot(x_pos,def(dn_index)*180/pi)
ylabel('\Delta\theta_z, [deg]')
xlabel('Beam station, L, [m]')



figure(2)
%----

subplot(3,2,1)
plot([x_pos],Fi(1:end-1,1))
ylabel('Normal force, Fx, [N]')
xlabel('Beam station, L, [m]')
title('Node forces in the local beam coordinate system')
grid on

subplot(3,2,2)
plot([x_pos],Mi(1:end-1,1))
ylabel('Twisting moment, Mx, [Nm]')
xlabel('Beam station, L, [m]')
grid on
%-----

subplot(3,2,3)
plot([x_pos],Fi(1:end-1,2))
ylabel('Shear force, Fy, [N]')
xlabel('Beam station, L, [m]')
grid on

subplot(3,2,4)
plot([x_pos],Mi(1:end-1,2))
ylabel('Bending moment, My, [Nm]')
xlabel('Beam station, L, [m]')
grid on

%----
subplot(3,2,5)
plot([x_pos],Fi(1:end-1,3))
ylabel('Shear force, Fz, [N]')
xlabel('Beam station, L, [m]')
grid on

subplot(3,2,6)
plot([x_pos],Mi(1:end-1,3))
ylabel('Bending moment, Mz, [Nm]')
xlabel('Beam station, L, [m]')
grid on

figure(3)
plot([x_pos],Total./10^6)
ylabel('von Mises Stress, \sigma, [MPa]')
xlabel('Beam station, L, [m]')
hold on
plot([0 x_pos(end)],[R_el R_el]./10^6,'r')
plot([0 x_pos(end)],[R_el R_el]./10^6/1.5,'r--')
legend('Beam stress','Yield stress','FAR Safety limit \eta=1.5')





figure(4)
subplot(3,2,1);

plot(x_pos,LF(dx_index))
ylabel('Node forces, F_X, [N]')
xlabel('Beam station, L, [m]')
title('Node forces in the global coordinate system')
subplot(3,2,3);
plot(x_pos,LF(dy_index))
ylabel('Node forces, F_Y, [N]')
xlabel('Beam station, L, [m]')

subplot(3,2,5);
plot(x_pos,LF(dz_index))
ylabel('Node forces, F_Z, [N]')
xlabel('Beam station, L, [m]')

subplot(3,2,2);
plot(x_pos,LF(dl_index)*180/pi)
ylabel('Node moment, M_X, [N]')
xlabel('Beam station, L, [m]')

subplot(3,2,4);
plot(x_pos,LF(dm_index)*180/pi)
ylabel('Node moment, M_Y, [Nn]')
xlabel('Beam station, L, [m]')

subplot(3,2,6);
plot(x_pos,LF(dn_index)*180/pi)
ylabel('Node moment, M_Z, [Nn]')
xlabel('Beam station, L, [m]')

figure(5)
T=input.profile.t*1000;
x=input.x;
plot(x,T)
ylabel('Skin Thickness, t, [mm]')
xlabel('Beam station, L, [m]')
axis([0 x(end) 0 max(T)*1.1])
title('Wing box wall thickness.')

figure(6)
h=input.profile.h*1000;
w=input.profile.w*1000;
x=input.x;
plot(x,h);
hold on
plot(x,w,'r');
ylabel('Dimensions, (h,w), [mm]')
xlabel('Beam station, L, [m]')
axis([0 x(end) 0 max(w)*1.1])
title('Wing box height and width.')
legend('Box height','Box width')

figure(7)
h=input.profile.h*1000;
w=input.profile.w*1000;
x=input.x;
%plot(x,h);
hold on
plot(x,h./w,'r');
ylabel('Thickness, [-]')
xlabel('Beam station, L, [m]')
%axis([0 x(end) 0 max(h./w)*1.1])
title('Wing box tickness ratio.')
%legend('Box height','Box width')

figure(99)
h3=plot3(mesh.GP_SB(:,1),mesh.GP_SB(:,2),mesh.GP_SB(:,3),'-o');
set(h3,'LineWidth',2)

hold on

JJ=mesh.GP_SB+input.trans;
h2=plot3(JJ(:,1),JJ(:,2),JJ(:,3),'r-d');
set(h2,'LineWidth',2)
%axis equal

