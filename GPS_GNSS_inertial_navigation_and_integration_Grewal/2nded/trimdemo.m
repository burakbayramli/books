%
% compares quaternions with and without trimming on random trajectory
%
clear all;
close all;
ratesdev = 1;    % attitude rate standard deviation in rad/sec
corrtime = 1;    % random rate correlation time in seconds
timestep = .001; % time step in seconds
Nsteps   = 10000; % number of time steps to simulate
%
phi      = exp(-timestep/corrtime);
noisdev  = ratesdev*sqrt(1-phi^2); % standard deviation of rate noise
rate     = ratesdev*randn(3,1);  % initial rotation rate vector (rad/sec)
Px       = qvec2mat([0;1;0;0]);
Py       = qvec2mat([0;0;1;0]);
Pz       = qvec2mat([0;0;0;1]);
Pxt      = Px;
Pyt      = Py;
Pzt      = Pz;
for k=1:Nsteps
   rotvec = timestep*rate;
   rate   = phi*rate + noisdev*randn(3,1);
   t(k)   = timestep*(k-1);
   x(k)   = rotvec(1);
   y(k)   = rotvec(2);
   z(k)   = rotvec(3);
   q      = rotvec2qvec(rotvec);
   Q      = qvec2mat(q);
   Px     = Q*Px/Q;
   Py     = Q*Py/Q;
   Pz     = Q*Pz/Q;
   M      = [Px(2:4,1),Py(2:4,1),Pz(2:4,1)];
   m11(k) = M(1,1);
   m12(k) = M(1,2);
   m13(k) = M(1,3);
   m21(k) = M(2,1);
   m22(k) = M(2,2);
   m23(k) = M(2,3);
   m31(k) = M(3,1);
   m32(k) = M(3,2);
   m33(k) = M(3,3);
   Pxt    = Q*Pxt/Q;
   Pyt    = Q*Pyt/Q;
   Pzt    = Q*Pzt/Q;
   Pxt    = quattrim(Pxt);
   Pyt    = quattrim(Pyt);
   Pzt    = quattrim(Pzt);
   Mt     = [Pxt(2:4,1),Pyt(2:4,1),Pzt(2:4,1)];
   err(k) = norm(M-Mt);
end;
plot(t,x,t,y,t,z);
legend('Roll','Pitch','Yaw');
xlabel('Time [sec]');
ylabel('Incremental Rotation [rad]');
title('Rotation History');
figure;
subplot(3,1,1),plot(t,m11,t,m12,t,m13);title('Direction Cosines');ylabel('1st Row');
subplot(3,1,2),plot(t,m21,t,m22,t,m23);ylabel('2nd Row');
subplot(3,1,3),plot(t,m31,t,m32,t,m33);ylabel('3rd Row');xlabel('Time [sec]');
figure;
plot(t,err);
xlabel('Time [sec]');
ylabel('Error [rad]')
