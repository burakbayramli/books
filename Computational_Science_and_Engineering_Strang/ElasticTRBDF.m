function ElasticTRBDF(T,dt,theta_init,thetadot_init,r_init,rdot_init,label)
%
%Elastic(T,dt,powers)
%----------------------------------------------------------------------------------- 
%Plots how the angles theta1 theta2 and their derivatives vary in time for
%the ELASTIC pendulum using the Trapezoidal scheme - assuming L=1, k=10, 
m=1
%Animated the elastic pendulum system with desired parameters using the TR-BDF2 Rule
% @ Sohan Dharmaraja MIT JUN2007

dt = dt/2;
powers = 1;
k=10;
m=1;
L=1;
timevec(1)=0;
thetavec(1)=theta_init;
thetadotvec(1)=thetadot_init;
rvec(1)=r_init;
rdotvec(1)=rdot_init;

xold=[theta_init; thetadot_init; r_init; rdot_init];
xnew=xold;

    N=T/dt;
    iter=1;
    
    timevec(1)=0;
    thetavec(1)=theta_init;
    thetadotvec(1)=thetadot_init;
    rvec(1)=r_init;
    rdotvec(1)=rdot_init;

    xold=[theta_init; thetadot_init; r_init; rdot_init];
    xnew=xold;

    f1=inline('iternew1 - (dt/2)*iternew2 - orig1 - (dt/2)*orig2','dt','orig1','orig2','iternew1','iternew2');
    f2=inline('iternew2 - (dt/2)*((2*iternew4*iternew2)/((-1)*iternew3) + ((9.81/((-1)*iternew3))*sin(iternew1))) - orig2 - (dt/2)*((2*orig4*orig2)/((-1)*orig3) + (((9.81/(-1*orig3)))*sin(orig1)))','dt','orig1','orig2','orig3','orig4','iternew1','iternew2','iternew3','iternew4');
    f3=inline('iternew3 - (dt/2)*iternew4 - orig3 - (dt/2)*orig4','dt','orig3','orig4','iternew3','iternew4');
    f4=inline('iternew4 - (dt/2)*((9.81*cos(iternew1) - (k/(m))*(iternew3-L) + iternew3*((iternew2)^2))) - orig4 - (dt/2)*((9.81*cos(orig1) - (k/(m))*(orig3-L) + orig3*((orig2)^2)))','dt','orig1','orig2','orig3','orig4','iternew1','iternew2','iternew3','iternew4','k','L','m');

    %define the 3pt BE scheme
    f1BE=inline('iternew1 - (4/3)*orig1older +(1/3)*orig1oldest - (2*dt/3)*iternew2','dt','orig1older','orig1oldest','iternew1','iternew2');
    f2BE=inline('iternew2 - (4/3)*orig2older +(1/3)*orig2oldest - (2*dt/3)*((2*iternew4*iternew2)/((-1)*iternew3) + ((9.81/((-1)*iternew3))*sin(iternew1)))','dt','orig2older','orig2oldest','iternew1','iternew2','iternew3','iternew4');
    f3BE=inline('iternew3 - (4/3)*orig3older +(1/3)*orig3oldest - (2*dt/3)*iternew4','dt','orig3older','orig3oldest','iternew3','iternew4');
    f4BE=inline('iternew4 - (4/3)*orig4older +(1/3)*orig4oldest - (2*dt/3)*((9.81*cos(iternew1) - (k/(m))*(iternew3-L) + iternew3*((iternew2)^2)))','dt','orig4older','orig4oldest','iternew1','iternew2','iternew3','iternew4','k','m','L');

posvec = 1;

for iter=1:2:N-1
posvec = posvec + 1;
xoldest = xnew;
xinit=xnew;

for j=1:5
    theta=xnew(1,1);
    V=xnew(2,1);
    r=xnew(3,1);
    W=xnew(4,1);
    
    J = [1, -1*(dt/2), 0, 0; (9.81/r)*cos(theta)*(dt/2), 1+((W/r)*dt), -1*(dt/2)*((9.81/(r^2))*sin(theta)+ (2*W*V)/(r^2)), V*(dt/r); 0, 0, 1, (-1)*(dt/2); (9.81*sin(theta))*(dt/2), (-1)*r*V*dt, (-1)*(V^2)*(dt/2)+(k/(2*m))*dt, 1];
    RHSvec(1,1)=f1(dt,xinit(1,1),xinit(2,1),xold(1,1),xold(2,1));
    RHSvec(2,1)=f2(dt,xinit(1,1),xinit(2,1),xinit(3,1),xinit(4,1),xold(1,1),xold(2,1),xold(3,1),xold(4,1));
    RHSvec(3,1)=f3(dt,xinit(3,1),xinit(4,1),xold(3,1),xold(4,1));
    RHSvec(4,1)=f4(dt,xinit(1,1),xinit(2,1),xinit(3,1),xinit(4,1),xold(1,1),xold(2,1),xold(3,1),xold(4,1),k,L,m);

    deltax=(J)\((-1)*RHSvec);
    xnew=xold+deltax;
    xold=xnew;


    % =====================================================================
    % =====================================================================
    
    xolder = xnew;
    for dotimes=1:5
        a=xnew(1,1);
        b=xnew(2,1);
        c=xnew(3,1);
        d=xnew(4,1);
        
        BETempmat=zeros(4,4);
        %Fill in the Jacobian element wise - easier debugging!
        
        BETempmat(1,1)=1;
        BETempmat(1,2)=-(2*dt/3);

        BETempmat(2,1)=-(2*dt/3)*(((9.81/((-1)*c))*cos(a)));
        BETempmat(2,2)=1-(2*dt/3)*((2*d*b)/((-1)*c));
        BETempmat(2,3)=(2*dt/3)*((2*d*b)/(c^2) + ((9.81/(c^2))*sin(a)));
        BETempmat(2,4)=(2*dt/3)*((2*b)/((-1)*c));
        
        BETempmat(3,3)=1;
        BETempmat(3,4)=-(2*dt/3);
        
        BETempmat(4,1)=(2*dt/3)*((9.81*sin(a)));
        BETempmat(4,2)=-(2*dt/3)*((2*c*((b))));
        BETempmat(4,3)=-(2*dt/3)*((- (k/m)*(c) + ((b)^2)));
        BETempmat(4,4)=1;
        
        RHSvec(1,1)=f1BE(dt,xolder(1,1),xoldest(1,1),xold(1,1),xold(2,1));
        RHSvec(2,1)=f2BE(dt,xolder(2,1),xoldest(2,1),xold(1,1),xold(2,1),xold(3,1),xold(4,1));
        RHSvec(3,1)=f3BE(dt,xolder(3,1),xoldest(3,1),xold(3,1),xold(4,1));
        RHSvec(4,1)=f4BE(dt,xolder(4,1),xoldest(4,1),xold(1,1),xold(2,1),xold(3,1),xold(4,1),k,m,L);
        
        deltax=(BETempmat)\((-1)*RHSvec);    
        xnew=xold+deltax;
        xold=xnew;
    end
    
    % =====================================================================
    % =====================================================================
end


thetavec(posvec)=xnew(1,1);
thetadotvec(posvec)=xnew(2,1);
rvec(posvec)=xnew(3,1);
rdotvec(posvec)=xnew(4,1);
timevec(posvec)=iter*dt;

subplot(2,2,1)
plot(timevec,thetavec,'LineWidth',2)
title('theta vs time','FontWeight','bold')

hold on

subplot(2,2,2)
plot(timevec,thetadotvec,'LineWidth',2)
title('theta dot vs time','FontWeight','bold')
hold on

subplot(2,2,3)
plot(timevec,rvec,'LineWidth',2)
title('r vs time','FontWeight','bold')
hold on

subplot(2,2,4)
plot(timevec,rdotvec,'LineWidth',2)
title('r dot vs time','FontWeight','bold')
hold on

store=[thetavec', thetadotvec'];
end

% now plot the exact solution using Matlab
% ------------------------------------------------------------------------------------------------- change this value below for spring constant 
%g = inline('[ y(2); (2*y(2)*y(4))/((-1*y(3))) + (9.81 *sin(y(1)))/(-1*y(3));y(4); 9.81*cos(y(1)) - ((k/m)*(y(3)-1)) + y(3)*((y(2))^2)]', 't','y','k','m');
%------------------------------------------------------------------------------------------------- ((k/m))*(R - Lnatural) change this value below for spring constant 
g = inline('[ y(2); (2*y(2)*y(4))/((-1*y(3))) + (9.81 *sin(y(1)))/(-1*y(3));y(4); 9.81*cos(y(1)) - ((10)*(y(3)-1)) + y(3)*((y(2))^2)]', 't', 'y');
[t,y]=ode45(g,[0,T],[theta_init; thetadot_init; r_init; rdot_init]);
% Can do - [t,y]=ode45(g,[0:0.1:2*pi],[theta_init;0]); if diff time steps required

subplot(2,2,1)
plot(t,y(:,1),'Color','red', 'LineWidth',2)
hold on

subplot(2,2,2)
plot(t,y(:,2),'Color','red', 'LineWidth',2)
hold on

subplot(2,2,3)
plot(t,y(:,3),'Color','red', 'LineWidth',2)
hold on

subplot(2,2,4)
plot(t,y(:,4),'Color','red', 'LineWidth',2)
hold off
 
initinfo = [num2str(T), ' ', num2str(2*dt), ' ', num2str(theta_init),' ',num2str(thetadot_init), ' ', num2str(r_init),' ', num2str(rdot_init)];
saveas(gcf,[num2str(label) ' TRBDF - ' initinfo '.png']);close;


