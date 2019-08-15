function demo4
% Restricted three-body problem by Arenstorf
% 
clc
example = 100;
while ~ismember(example,[1,2,3])
   example = input(' Example No. (1/2/3) ');
end;
clf
switch example
case 1, startwert = [1.2;0;0;-1.049357];     
        period = 6.192169; MU = 0.012277471;
case 2, startwert = [0.994;0;0;-2.031732629];
        period = 11.124340; MU = 0.012277471;
case 3, startwert = [0.994;0;0;-2.113898];
        period = 5.436795; MU = 0.012277471;
end
Parmeter = MU;
options = odeset('Reltol',1E-8);

[T,Y] = ode45(@bsp04,[0,period],startwert,options,Parmeter);
%plot(-1.5,-1.5,'k.','markersize',3),hold on
%plot(1.5,1.5,'k.','markersize',3),hold on
%axis equal tight, axis manual, grid on
plot(Y(:,1),Y(:,2),'linewidth',2);

