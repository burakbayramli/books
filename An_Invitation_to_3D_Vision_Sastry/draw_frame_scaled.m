% Draw frame at some specified position and specified orientation 
% function[] = draw_frame_scaled(T,scale) 
% T - (4x4) rigid body transformation homogeneous matrix T
% scale - specifies the lenght of the axis
% Jana Kosecka, GMU, June 2003

function[] = draw_frame_scaled(T,scale)    
pos = T(1:3,4);
plot3(pos(1),pos(2),pos(3),'k.');

% coordinates of the vectors in the init frame
i0 = [1 0 0]';
j0 = [0 1 0]';
k0 = [0 0 1]';

i = scale*T(1:3,1:3)*i0 + pos;
j = scale*T(1:3,1:3)*j0 + pos;
k = scale*T(1:3,1:3)*k0 + pos;


vx = scale*T(1:3,1:3)*i0/norm(T(1:3,1:3)*i0);
vy = scale*T(1:3,1:3)*j0/norm(T(1:3,1:3)*j0);
vz = scale*T(1:3,1:3)*k0/norm(T(1:3,1:3)*k0);

h = quiver3(pos(1),pos(2),pos(3),vx(1),vx(2),vx(3),0);
% set(h(:),'LineWidth',1.5); 
set(h(:),'Color','black'); 
h = quiver3(pos(1),pos(2),pos(3),vy(1),vy(2),vy(3),0);
% set(h(:),'LineWidth',1.5); 
set(h(:),'Color','black'); 
% thicker line is the optical axis
h = quiver3(pos(1),pos(2),pos(3),vz(1),vz(2),vz(3),0);
set(h(:),'Color','red'); 


