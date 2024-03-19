%Source Panel Method
%Example 3.19 of Fundamentals of Aerodynamics by J.D. Anderson
%Code by: Dr. Bilal A. Siddiqui,
%Asst. Prof., Mechanical Engineering, DHA Suffa University
clc;clear all;close all;
Vinf=30;
R=1;%cylinder radius
n=100;%number of panels
dtheta=2*pi/n;
alfa=0;%angle of attack;
theta=pi+pi/n:-dtheta:-pi+pi/n;%central angle
X=R*cos(theta);
Y=R*sin(theta);
for index=1:n
    %angle of flow with tangent of panel
    phi(index)=-alfa+...
        atan2((Y(index+1)-Y(index)),(X(index+1)-X(index)));
    %angle of flow with normal of panel
    beta(index)=phi(index)+pi/2;
    midpoint_x(index)=(X(index+1)+X(index))/2;
    midpoint_y(index)=(Y(index+1)+Y(index))/2;
    S(index)=sqrt((Y(index+1)-Y(index))^2+...
        (X(index+1)-X(index))^2);%length of panel
end
%The Source Panel Method
for p=1:n
    neighbors(:,p)=[1:p-1 p+1:n];
    xi=midpoint_x(p);
    yi=midpoint_y(p);
    for index=1:n-1
        m=neighbors(index,p);
        Xj=X(m);
        Yj=Y(m);
        Xj1=X(m+1);
        Yj1=Y(m+1);
        A=-(xi-Xj)*cos(phi(m))-(yi-Yj)*sin(phi(m));
        B=(xi-Xj)^2+(yi-Yj)^2;
        C=sin(phi(p)-phi(m));
        D=(yi-Yj)*cos(phi(p))-(xi-Xj)*sin(phi(p));
        E=sqrt(B-A^2);
        Sj=S(m);
        I(p,m)=C/2*log((Sj^2+2*A*Sj+B)/B)+...
            (D-A*C)/E*(atan2((Sj+A),E)-atan2(A,E));
        J(p,m)=(D-A*C)/2/E*log((Sj^2+2*A*Sj+B)/B)...
            -C*(atan2((Sj+A),E)-atan2(A,E));
    end
    F(p,1)=Vinf*cos(beta(p));
end

M=I/2/pi+eye(n)/2;

lambda=-inv(M)*F;
fprintf('The sum of all sources is %f',lambda'*S');%check sum

%Recoving velocity at the nodes
V=Vinf*sin(beta)+lambda'/2/pi*J';

Cp=1-(V/Vinf).^2;
angles=min(beta):0.01:max(beta);
Cp_exact=1-4*sin(angles).^2;
subplot(1,2,1);plot(R*cos(0:0.01:2*pi),R*sin(0:0.01:2*pi),'b',...
    X,Y,'r',midpoint_x,midpoint_y,'g^');axis equal;
legend('Exact Shape','Panel approximation','Control Points')
subplot(1,2,2);plot(angles,Cp_exact,'b',beta,Cp,'r^');grid;
legend('C_p (exact', 'C_p (Source Panel Method)');
