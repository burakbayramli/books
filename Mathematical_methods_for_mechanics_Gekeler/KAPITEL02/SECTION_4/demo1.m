function demo1
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% Arenstorf-Orbits mit dopri.m, vgl. Hairer II
% x = [x; y; xprime; yprime]
clear, clc
disp(' Arenstorf-Orbits ')
example= 100;
while ~ismember(example,[1,2,3])
   example = input(' Example No. (1/2/3) ');
end
% ------------------------------------------------------
switch example
case 1
   Eps = 1e-5; tau_max = 2; tau = 0.1;
   t0  = 0; t_end = 6.192169; % Periode
   x0  = [1.2;0;0;-1.049357]; %Startwert
   mue = 0.012277471;
   % mue = 0.01212856276;
   parmtr2 = [Eps,tau_max,tau]; % Parameter fuer Verfahren
   parmtr3 = mue;          % Parameter fuer DGl
   [xout,errorcode] = dopri('dreik_a',t0,x0,t_end,parmtr2,parmtr3);
   save daten xout
case 2
   Eps = 1e-5; tau_max = 2; tau = 0.1;
   t0  = 0; t_end = 11.124340; % Periode
   x0  = [0.994;0;0;-2.031732];
   mue = 0.012277471;
   parmtr2 = [Eps,tau_max,tau]; % Parameter fuer Verfahren
   parmtr3 = mue;          % Parameter fuer DGl
   [xout,errorcode] = dopri('dreik_a',t0,x0,t_end,parmtr2,parmtr3);
   save daten xout
case 3
   Eps = 1e-5; tau_max = 2; tau = 0.1;
   t0  = 0; t_end = 5.436795; % Periode
   x0  = [0.994;0;0;-2.113898];
   mue = 0.012277471;
   parmtr2 = [Eps,tau_max,tau]; % Parameter fuer Verfahren
   parmtr3 = mue;          % Parameter fuer DGl
   [xout,out] = dopri('dreik_a',t0,x0,t_end,parmtr2,parmtr3);
   save daten xout
end
fig00
