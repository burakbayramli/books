% Canonical error surface
v0=-2:0.02:2;
v1=-2:0.02:2;
[V0,V1]=meshgrid(v0,v1);

Ry=[3.8 0.9;0.9 3.8];
rxy=[1.7;0.6];

hop=Ry\rxy; %optimal h
Emin=14-(hop'*rxy); %minimum error

[Q,D]=eig(Ry); %eigen

aux1=(D(1,1)*V0.^2)+(D(2,2)*V1.^2);
ers=Emin+aux1;



%display---------------------------

figure(1)
contour(V0,V1,ers,20); %plots figure
xlabel('v(0)'); ylabel('v(1)');title('error surface (contour)');

