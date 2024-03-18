function[]=FEMresultplot2(input,mesh)

Fi   =input.Fi;
Mi   =input.Mi;
Total=input.Total;
def  =input.def;
R_el =input.R_el;
LF   =input.Loads;

% Plotting results
% Computing indices
[a b]=size(def)

dx_index=1:6:(a-5);
np = length(dx_index)
nSB = np/2;
nP  = np-nSB;
dx = def(dx_index);
dy_index=2:6:(a-4);
dy = def(dy_index);
dz_index=3:6:(a-3);
dz = def(dz_index);
dl_index=4:6:(a-2);
dl = def(dl_index);
dm_index=5:6:(a-1);
dm = def(dm_index);
dn_index=6:6:(a-0);
dn = def(dn_index);

x_pos=input.x;
length(x_pos)
figure(1)

subplot(3,2,1);

plot(x_pos,[flipud(dx(nP+1:end));(dx(1:nP))])
ylabel('\Delta x, [m]')
xlabel('Beam station, L, [m]')
title('Deformation in the global coordinate system')

subplot(3,2,3);
plot(x_pos,[flipud(dy(nP+1:end));(dy(1:nP))])
ylabel('\Delta y, [m]')
xlabel('Beam station, L, [m]')

subplot(3,2,5);
plot(x_pos,[flipud(dz(nP+1:end));(dz(1:nP))])
ylabel('\Delta z, [m]')
xlabel('Beam station, L, [m]')

subplot(3,2,2);
plot(x_pos,[flipud(dl(nP+1:end));(dl(1:nP))]*180/pi)
ylabel('\Delta\theta_x, [deg]')
xlabel('Beam station, L, [m]')

subplot(3,2,4);
plot(x_pos,[flipud(dm(nP+1:end));(dm(1:nP))]*180/pi)
ylabel('\Delta\theta_y, [deg]')
xlabel('Beam station, L, [m]')

subplot(3,2,6);
plot(x_pos,[flipud(dn(nP+1:end));(dn(1:nP))]*180/pi)
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
plot(x_pos(1:length(T)),T)
ylabel('Skin Thickness, t, [mm]')
xlabel('Beam station, L, [m]')
%axis([0 x_pos(length(T)) 0 max(T)*1.1])
title('Wing box wall thickness.')

figure(6)
h=input.profile.h*1000;
w=input.profile.w*1000;
plot(x_pos(1:length(h)),h);
hold on
plot(x_pos(1:length(h)),w,'r');
ylabel('Dimensions, (h,w), [mm]')
xlabel('Beam station, L, [m]')
%axis([0 x_pos(length(h)) 0 max(w)*1.1])
title('Wing box height and width.')
legend('Box height','Box width')

figure(7)
hold on
plot(x_pos(1:length(h)),h./w,'r');
ylabel('Thickness, [-]')
xlabel('Beam station, L, [m]')
%axis([0 x(end) 0 max(h./w)*1.1])
title('Wing box tickness ratio.')
%legend('Box height','Box width')

figure(99)
H     = mesh.profile.h;
W     = mesh.profile.w;
bxyz  = mesh.GP_SB';
r_mat = mesh.stiffness_rotation_matrix(1:3,1:3,:);
nHW   = length(H);
rct0 = [ 0  0  0  0;...
        -1  1  1 -1;...   
        -1 -1  1  1];
rct = cell(1,nHW);
trns = input.trans; % global coords!
for i = 1:nHW
    rotmat = squeeze(r_mat(:,:,i));
    rectt  = rotmat'*(diag([1 W(i)/2 H(i)/2])*rct0)+bxyz(:,i)*ones(1,4);
    patch(rectt(1,:),rectt(2,:),rectt(3,:),'y');
    rct{i}=rectt;
    hold on
end
h3=plot3(mesh.GP_SB(:,1),mesh.GP_SB(:,2),mesh.GP_SB(:,3),'-o');
set(h3,'LineWidth',2)

hold on
nnn = size(bxyz,2);
JJ=mesh.GP_SB+trns(1:nnn,:);
h2=plot3(JJ(:,1),JJ(:,2),JJ(:,3),'r-d');
set(h2,'LineWidth',2)
axis equal

