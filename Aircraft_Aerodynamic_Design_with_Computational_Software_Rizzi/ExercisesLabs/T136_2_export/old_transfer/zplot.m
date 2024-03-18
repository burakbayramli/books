%Zplot
%This function is a plotting help script
figure(2)
h=plot(-flipud(out.ypstation(1:31)),(out.shear(1:31)));
hold on
set(h,'Linewidth',2)
ylabel('Shear force, F_z, [N]')
xlabel('Span station, y, [m]')
grid on

figure(3)
h=plot(-flipud(out.ypstation(1:31)),(out.Bend(1:31)));
set(h,'Linewidth',2)
xlabel('Span station, y, [m]')
ylabel('Bend moment, M_x, [Nm]')
grid on
hold on

figure(4)
h=plot(-flipud(out.ypstation(1:31)),(out.Twist(1:31)))
set(h,'Linewidth',2)
grid on
ylabel('Twist moment, M_y, [Nm]')
xlabel('Span station, y, [m]')
hold on