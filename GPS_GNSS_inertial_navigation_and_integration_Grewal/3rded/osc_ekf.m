%
% osc_ekf.m
%
% Demonstration of extended Kalman filter
% for tracking a harmonic oscillator
% with drifting dynamic characteristics.
%
% STATE VARIABLES
% x(1) is cosine phase component of signal
% x(2) is sine phase component of signal
% x(3) is damping time-constant of oscillator (nominally 5 sec)
% x(4) is frequency of oscillator (nominally 2pi rad/sec, or 1 Hz)
%
clear all;
close all;
disp('Nonlinear Kalman Filter Tracking Time-Varying Damped Oscillator');
simtime = input('How many seconds do you want to simulate? (1-10 is good):');
if simtime>10
   disp('Ten seconds will be more than enough');
   simtime = 10;
elseif simtime<1
   disp('One second will be more meaningful');
   simtime = 1;
end;
dt = .01;                  % time step
t  = 0;                    % initial time
xt = [randn;randn;5+randn/2;2*pi+randn/3]; % simulated true state
x  = [0;0;5;2*pi];         % initial estimates of state variables
R  = 1e-6;                 % sensor noise covariance
H  = [1,0,0,0];            % measurement sensitivity matrix
P  = [1,0,0,0;0,1,0,0;0,0,.25,0;0,0,0,.36];
Q  = zeros(4);
Q(1,1)  = 1 - exp(-2*dt/5);
Q(2,2)  = 1 - exp(-2*dt/5);
Q(3,3)  = 1 - exp(-2*dt/20);
Q(4,4)  = 1 - exp(-2*dt/60);
tyme    = [t];
X1      = [x(1)];
X2      = [x(2)];
X3      = [x(3)];
X4      = [x(4)];
X1t     = [xt(1)];
X2t     = [xt(2)];
X3t     = [xt(3)];
X4t     = [xt(4)];
for k=1:round(simtime/dt),
   zhat= H*x;
   z   = H*xt + sqrt(R)*randn;
   K   = P*H'/(H*P*H'+R);  % Kalman gain
   x   = x + K*(z-zhat);   % corrected state vector
   P   = P - K*H*P;        % corrected covariance
   P   = .5*(P+P');        % symmetrized
   tyme  = [tyme,t];
   X1    = [X1,x(1)];   
   X2    = [X2,x(2)];   
   X3    = [X3,x(3)];   
   X4    = [X4,x(4)];   
   X1t   = [X1t,xt(1)];   
   X2t   = [X2t,xt(2)];   
   X3t   = [X3t,xt(3)];   
   X4t   = [X4t,xt(4)];   
   F   = [-1/x(3),x(4),x(1)/x(3)^2,x(2);-x(4),-1/x(3),x(2)/x(3)^2,-x(1);0,0,-1/60,0;0,0,0,-1/20];
   Phi = eye(4) + dt*F + (dt^2/2)*F^2;
   [T,X] = ode45('dharmosc',[t t+dt],x); % integrate the nonlinear equations
   [rows,cols] = size(X);
   x   = X(rows,:)'; % predicted state vector
   xt(3) = x(3) + dt*(-x(3)+5)/60 + sqrt(Q(3,3))*randn;
   xt(4) = x(4) + dt*(-x(4)+2*pi)/20 + sqrt(Q(4,4))*randn;
   c     = cos(dt*xt(4));
   s     = sin(dt*xt(4));
   e     = exp(-dt/x(3));
   xt1   = e*(c*xt(1) + s*xt(2)) + sqrt(Q(1,1))*randn;
   xt(2) = e*(-s*xt(1) + c*xt(2))+ sqrt(Q(2,2))*randn;
   xt(1) = xt1;
   P     = Phi*P*Phi' + Q; % predicted covariance
   t     = t+dt;
   tyme  = [tyme,t];
   X1    = [X1,x(1)];   
   X2    = [X2,x(2)];   
   X3    = [X3,x(3)];   
   X4    = [X4,x(4)];   
   X1t   = [X1t,xt(1)];   
   X2t   = [X2t,xt(2)];   
   X3t   = [X3t,xt(3)];   
   X4t   = [X4t,xt(4)];   
   if abs(t-round(t))<dt/2
      disp([num2str(t),' sec simulated time']);
   end;
end;
figure;
subplot(4,1,1),plot(tyme,X1,'r:',tyme,X1t,'b-');
title('Nonlinear Kalman Filter Tracking Time-Varying Damped Oscillator');
ylabel('In-Phase');
subplot(4,1,2),plot(tyme,X2,'r:',tyme,X2t,'b-');
ylabel('Quadrature');
subplot(4,1,3),plot(tyme,X3,'r:',tyme,X3t,'b-');
ylabel('tau');
legend('est.','true');
subplot(4,1,4),plot(tyme,X4,'r:',tyme,X4t,'b-');
ylabel('omega');
xlabel('Time [sec]');
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  
