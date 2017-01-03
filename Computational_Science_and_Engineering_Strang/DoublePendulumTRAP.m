function RetMatrix = DoublePendulumTRAP(T,dt,theta1_init,theta1dot_init,theta2_init,theta2dot_init,m1,m2,L1,L2,maxiters,Doplot,DoplotEig)
%
%Plotpen2(T,dt,theta1_init,theta1dot_init,theta2_init,theta2dot_init,m1,m2,L1,L2)
%----------------------------------------------------------------------------------- 
%Animated the rigid coupled pendulum system with desired parameters using
%Trapezoidal Rule
% @ Sohan Dharmaraja MIT OCT2006

% Variables we're using:
% a = theta1
% b = theta2
% p = m1
% q = m2
% i = L1
% j = L2
% X = theta1dot
% Y = theta2dot

%Vectors we're defining:
% timevec - times
% avec - to store the angle theta1
% bvec - to store the angle theta2
% adotvec - to store the velocity, theta1dot
% bdotvec - to store the velocity, theta2dot

%set up initial conditions
timevec(1)=0;
avec(1)=theta1_init;
adotvec(1)=theta1dot_init;
bvec(1)=theta2_init;
bdotvec(1)=theta2dot_init;
p=m1;
q=m2;
i=L1;
j=L2;
g=9.81;
N=T/dt;
Totiters=0;

xold=[theta1_init; theta1dot_init; theta2_init; theta2dot_init];
xnew=xold;

    %define the Trapezoidal scheme
    f1=inline('iternew1 - (dt/2)*iternew2 - orig1 - (dt/2)*orig2','dt','orig1','orig2','iternew1','iternew2');
    f2=inline('iternew2 - (dt/2)*(-9.81*(2*p + q)* sin(iternew1) - q*9.81*sin(iternew1 - 2*iternew3) - 2*sin(iternew1-iternew3)*q*((iternew4^2)*j + (iternew2^2)*i*cos(iternew1-iternew3)))/(i*(2*p + q - q*cos(2*iternew1-2*iternew3))) - orig2 - ((dt/2)*(-9.81*(2*p + q)* sin(orig1) - q*9.81*sin(orig1 - 2*orig3) - 2*sin(orig1-orig3)*q*((orig4^2)*j + (orig2^2)*i*cos(orig1-orig3))))/(i*(2*p + q - q*cos(2*orig1-2*orig3)))','dt','orig1','orig2','orig3','orig4','iternew1','iternew2','iternew3','iternew4','p','q','i','j');
    f3=inline('iternew3 - (dt/2)*iternew4 - orig3 - (dt/2)*orig4','dt','orig3','orig4','iternew3','iternew4');
    f4=inline('(iternew4 - ((dt/2)*(2*sin(iternew1-iternew3)*((iternew2^2)*i*(p+q) + 9.81*(p+q)*cos(iternew1) + (iternew4^2)*j*q*cos(iternew1-iternew3))))/(j*(2*p + q - q*cos(2*iternew1 - 2*iternew3)))) - orig4 - ((dt/2)*(2*sin(orig1-orig3)*((orig2^2)*i*(p+q) + 9.81*(p+q)*cos(orig1) + (orig4^2)*j*q*cos(orig1-orig3))))/(j*(2*p + q - q*cos(2*orig1 - 2*orig3)))','dt','orig1','orig2','orig3','orig4','iternew1','iternew2','iternew3','iternew4','p','q','i','j');

flagval=0;

%Let user know how far along the calculation is!
h = waitbar(0,'Calculating for the Trap scheme... Press ENTER afterwards to start animation');

tic
if DoplotEig =='Y'
    figure;
end

for iter=1:N
waitbar(iter/N)
xinit=xnew;
%Zero the Jacobian
J=zeros(4,4);

% DO FIVE NEWTON ITERATIONS: this can be changed for performance, if
% desired, but 5 does a fairly good job. Tolerance checking alone causes
% slight slow down...
delxvec=1;
deltax=1;
counter=0;

%dotimes=1;

% while norm(deltax)>=(10*eps)
%for dotimes=1:maxiters
for reptimes=1:maxiters
    
    Totiters=Totiters+1;
    counter=counter+1;
    a=xnew(1,1);
    X=xnew(2,1);
    b=xnew(3,1);
    Y=xnew(4,1);

    Tempmat=zeros(4,4);
    %Fill in the Jacobian element wise - easier debugging!
    Tempmat(1,2)=1;
    Tempmat(2,1)=(-g*(2*p + q)*cos(a) -q*g*cos(a-2*b) - 2*cos(a-b)*q*((Y^2)*j+(X^2)*i*cos(a-b)) + 2*((sin(a-b))^2)*q*i*(X^2)) / (i*(2*p +q -q*(cos(2*a - 2*b)))) - ((2*(-g*(2*p + q)*sin(a) -q*g*sin(a-2*b) - 2*sin(a-b)*q*((Y^2)*j+(X^2)*i*cos(a-b)))*q*sin(2*a-2*b)) / (i*(((2*p +q -q*(cos(2*a - 2*b))))^2)));
    Tempmat(2,2)=-(4*sin(a-b)*q*X*cos(a-b))/(2*p +q -q*(cos(2*a - 2*b)));
    Tempmat(2,3)=(2*q*g*cos(a - 2*b) + 2*cos(a-b)*q*((Y^2)*j+(X^2)*i*cos(a-b)) - 2*((sin(a-b))^2)*q*i*(X^2)) /  (i*(2*p +q -q*(cos(2*a - 2*b))))                 + (2*(-g*(2*p + q)*sin(a) -q*g*sin(a-2*b) - 2*sin(a-b)*q*((Y^2)*j+(X^2)*i*cos(a-b)))*q*sin(2*a-2*b)) / (i*(((2*p +q -q*(cos(2*a - 2*b))))^2));
    Tempmat(2,4)=-(4*sin(a-b)*q*Y*j)/(i*(2*p +q -q*(cos(2*a - 2*b))));
    Tempmat(3,4)=1;
    Tempmat(4,1)=((((2*cos(a-b)*((X^2)*i*(p+q) + g*(p+q)*cos(a) + (Y^2)*j*q*cos(a-b))))/(j*(2*p + q - q*cos(2*a - 2*b))))) + ((2*sin(a-b)*(-g*(p+q)*sin(a) -(Y^2)*j*q*sin(a-b)))/(j*(2*p + q - q*cos(2*a - 2*b)))) -((4*sin(a-b)*((X^2)*i*(p+q) + g*(p+q)*cos(a) + (Y^2)*j*q*cos(a-b)))*q*sin(2*a-2*b)/(j*(((2*p + q - q*cos(2*a - 2*b))^2))));
    Tempmat(4,2)=(4*sin(a-b)*X*i*(p+q))/(j*(2*p + q - q*cos(2*a - 2*b)));
    Tempmat(4,3)=-((2*cos(a-b)*((X^2)*i*(p+q) + g*(p+q)*cos(a) + (Y^2)*j*q*cos(a-b)))/(j*(2*p + q - q*cos(2*a - 2*b)))) + ((2*((sin(a-b))^2)*(Y^2)*q)/(2*p + q - q*cos(2*a - 2*b)))     +((4*sin(a-b)*((X^2)*i*(p+q) + g*(p+q)*cos(a) + (Y^2)*j*q*cos(a-b)))*q*sin(2*a-2*b)/(j*(((2*p + q - q*cos(2*a - 2*b))^2))));
    Tempmat(4,4)=(4*sin(a-b)*Y*q*cos(a-b))/(2*p + q- q*cos(2*a - 2*b));

    % Compute the Jacobian to be used for the Newton iteration
    J=eye(4)-(dt/2)*Tempmat;
    RHSvec(1,1)=f1(dt,xinit(1,1),xinit(2,1),xold(1,1),xold(2,1));
    RHSvec(2,1)=f2(dt,xinit(1,1),xinit(2,1),xinit(3,1),xinit(4,1),xold(1,1),xold(2,1),xold(3,1),xold(4,1),p,q,i,j);
    RHSvec(3,1)=f3(dt,xinit(3,1),xinit(4,1),xold(3,1),xold(4,1));
    RHSvec(4,1)=f4(dt,xinit(1,1),xinit(2,1),xinit(3,1),xinit(4,1),xold(1,1),xold(2,1),xold(3,1),xold(4,1),p,q,i,j);
    
    deltax=inv(J)*((-1)*RHSvec);
    %det(inv(J));
    
    if DoplotEig=='Y'
    v=eig(J);
    plot(v,'bo','MarkerEdgeColor','k','MarkerFaceColor',[0 0 0],'MarkerSize',2)
    hold on
    end
    
    xnew=xold+deltax;
    xold=xnew;
    delxvec(counter)=norm(deltax);
end

%Update our vectors with the new values: theta1, theta1dot, theta2, theta2dot
avec(iter+1)=xnew(1,1);
adotvec(iter+1)=xnew(2,1);
bvec(iter+1)=xnew(3,1);
bdotvec(iter+1)=xnew(4,1);
timevec(iter+1)=iter*dt;

end

if DoplotEig=='Y'
    
    v=[-pi:0.01:pi];
    circ=cos(v)+sqrt(-1)*sin(v);
    plot(circ)
    axis([0.7 1.3 -0.4 0.4])
    hold off
    title('Eigenvalues and the unit circle - Trapezoidal Rule')
    figure;
end

close(h)

timereq=toc;
fprintf('The time required using the trapezoidal-rule integration method was %u seconds.\n',timereq)
fprintf('%u iterations were performed!\n',Totiters)


modavec = avec;
modbvec = bvec;

theta1info = [num2str(T),' ', num2str(dt),' ', num2str(theta1_init),' ', num2str(theta1dot_init), ' '];
theta2info = [num2str(theta2_init),' ', num2str(theta2dot_init), ' '];

figure;
plot(modavec,adotvec,'LineWidth',1);xlabel('Angular displacement - \theta_{1}'); ylabel('Angular velocity - \theta_{1}'); title('Poincare Plot - Pendulum 1')
saveas(gcf,['Trap - Theta1 ' theta1info ' ' theta2info '.png']); close; figure;
plot(modbvec,bdotvec,'LineWidth',1);xlabel('Angular displacement - \theta_{2}'); ylabel('Angular velocity - \theta_{2}'); title('Poincare Plot - Pendulum 2')
saveas(gcf,['Trap - Theta2 ' theta1info ' ' theta2info '.png']); close; figure;

if Doplot=='Y'
    figure;

%Draw the axes where the animation will be
set(gca,'XLim',[-(i+j) (i+j)],'YLim',[-(i+j) (i+j)],'XTick',[-(i+j):(i+j)],'YTick',[-(i+j):(i+j)],'Drawmode','fast','Visible','on','NextPlot','add');

plot(0,0,'ks')
grid on
axis equal
axis manual

bob1 = line('color',[0.1 0.8 0.1],'Marker','.','markersize',35,'erase','xor','xdata',[],'ydata',[]);
rod1 = line('color',[0.4 0.4 1],'LineStyle','-','LineWidth',2.5,'erase','xor','xdata',[],'ydata',[]);
bob2 = line('color',[0.1 0.8 0.1],'Marker','.','markersize',35,'erase','xor','xdata',[],'ydata',[]);
rod2 = line('color',[0.4 0.4 1],'LineStyle','-','LineWidth',2.5,'erase','xor','xdata',[],'ydata',[]);

%Animate!
tic
for Curtime=0:(length(avec)-1)
    
  xbob1=i*sin(avec(Curtime+1));
  ybob1 = -i*cos(avec(Curtime+1));
  xrod1 = [0 xbob1]; yrod1 = [0 ybob1];
  
  xbob2=xbob1+ j*sin(bvec(Curtime+1));
  ybob2=ybob1+ -j*cos(bvec(Curtime+1));
  xrod2 = [xbob1 xbob2]; yrod2 = [ybob1 ybob2];
  
  set(bob1,'xdata',i*sin(avec(Curtime+1)),'ydata',-i*cos(avec(Curtime+1)))
  set(bob2,'xdata',i*sin(avec(Curtime+1))+ j*sin(bvec(Curtime+1)),'ydata',-i*cos(avec(Curtime+1))-j*cos(bvec(Curtime+1)))   
  set(rod1,'xdata',xrod1,'ydata',yrod1)
  set(rod2,'xdata',xrod2,'ydata',yrod2)
  
    drawnow; 
    if flagval==0
      flagval=1;
        saveas(gcf,['DPgridon' theta1info ' ' theta2info '.png']);
        grid off
        saveas(gcf,['DPgridoff' theta1info ' ' theta2info '.png']);
        grid on
      pause
    end

  pause(dt)

end
toc
end

RetMatrix=[timevec',avec',adotvec',bvec',bdotvec'];