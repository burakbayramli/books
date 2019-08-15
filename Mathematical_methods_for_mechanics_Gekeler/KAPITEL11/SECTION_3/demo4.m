function demo4
disp(' Roboter nach Schiehlen ')
clear, clc, format compact
nr = 10; KK = [1];
%while ~ismember(nr,KK)
%   nr   = input(' Beispiel Nr. (1) ');
%   done = ismember(nr,K);
%end;
%warning off MATLAB:ode23t:IntegrationTolNotMet
%options = odeset('RelTol',1E-3,'MaxStep',0.01,'Mass','bsp05b', ...
% 'massSingular','maybe','Initialslope',X0P,'MStateDependence','strong');
options = odeset('RelTol',1E-4,'MaxStep',0.005,'Mass','bsp04b', ...
'massSingular','no','MStateDependence','strong');

X0A      = [2.25; -0.5236; 0.75; 0; 0]; X0B = zeros(5,1);
X0       = [X0A;X0B]; T_END    = 2; Parmeter = [];
[T,Y]    = ode23t(@bsp04a,[0, T_END],X0,options,Parmeter);
save daten T Y Parmeter
bld060805
