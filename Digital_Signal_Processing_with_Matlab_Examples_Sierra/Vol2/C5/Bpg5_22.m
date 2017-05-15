% Steepest-descent steps
x1=-2:0.02:2;
x2=-2:0.02:2;

[X1,X2]=meshgrid(x1,x2);

A=[3.8 0.9;0.9 3.8];
b=[1.7;0.6];

aux1=(A(1,1)*X1.^2)+(A(1,2)*X1.*X2)+(A(2,1)*X2.*X1)+(A(2,2)*X2.^2);
aux2=(X1*b(1))+(X2*b(2));
ers=14+aux1-2*aux2;

Np=30; %number of steps
%space for the record of steps
vx1=zeros(1,Np);
vx2=zeros(1,Np);
ver=zeros(1,Np);

%doing the steps
px=[-1.5;1.5]; %initial point
mu=0.02; %step size

for nn=1:Np,
   %storing:
   vx1(nn)=px(1);
   vx2(nn)=px(2);
   aux1=px'*A*px; aux2=2*px'*b;
   ver(nn)=14+aux1-2*aux2;
   %next step
   aux=b-(A*px);
   px=px+(mu*aux);
end;   

%display---------------------------
figure(1)
mesh(X1,X2,ers); hold on; %plots figure
view(30,30);
plot3(vx1,vx2,ver,'-ok');
xlabel('x1'); ylabel('x2');title('error surface');
figure(2)
contour(X1,X2,ers,20); hold on; %plots figure
plot(vx1,vx2,'-ok');
xlabel('x1'); ylabel('x2');title('steps on the error plot');

