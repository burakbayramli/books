% Error surface
h0=-2:0.02:2;
h1=-2:0.02:2;
[H0,H1]=meshgrid(h0,h1);

Ry=[3.8 0.9;0.9 3.8];
rxy=[1.7;0.6];

aux1=(Ry(1,1)*H0.^2)+(Ry(1,2)*H0.*H1)+(Ry(2,1)*H1.*H0)+(Ry(2,2)*H1.^2);
aux2=(H0*rxy(1))+(H1*rxy(2));
ers=14+aux1-2*aux2;

%display---------------------------
figure(1)
mesh(H0,H1,ers); %plots figure
xlabel('h(0)'); ylabel('h(1)');title('error surface');

figure(2)
contour(H0,H1,ers,20); %plots figure
xlabel('h(0)'); ylabel('h(1)');title('error surface (contour)');

