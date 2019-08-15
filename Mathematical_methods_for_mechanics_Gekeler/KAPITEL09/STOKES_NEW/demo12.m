function demo12
% E.W. Gekeler, Release 09.09.09 
% Navier-Stokes Problem: LETTERS F E M
% Linear, Taylor-Hood elements
% X,Y : coordinates
% U,V :  velocity in x- and y-direction
% V   : velocity in y-direction
% P   : Pressure (PRESSURE FAILS WITHOUT PENALTY TERM!!!)
% p,e,t:  nodes,edges,triangles
% p1,t1: data of intermediate nodes and their indices in triangles
% e is augmented by an eighth row with indices of midpoints in [p,p1]

clear, clc, format short, format compact
% Example: 
FF1 = 'bsp08'; FF2 = 'bsp08h';
% -- Parameters ---------------
NU       = 0.01; % coeff. of viscosity [m*m/sec] at Re=100
F        = 0;    % load
EPSILON  = 1E-3; % Penalty Term
Parmeter = [NU,F,EPSILON];
REFINE   = 2; % Nr. of mesh refinements
SEGNR    = [1,2,3,4];
% -----------------------------
Start = 100; KK = [0,1];
%while ~ismember(Start,KK)
%   Start = input(' New start or Restart ? (1/0) ');
%end
Start = 1;
if Start == 1
   [p,e] = feval(FF1);
   [t,FORMED] = mesh11_2(p,e);
   t = [t;ones(1,size(t,2))]; % row no. four
   for J = 1:REFINE
      disp(' Refinemesh ')
      [p,e,t] = mesh01_t([],p,e,t); % FF2 = [] !!
  %   [p,t] = mesh10(p,e,t);
      p = mesh10(p,e,t);
   end
   disp(' midpoints ')
   [p1,e,t1]  = mesh06_t(p,e,t);
   save daten12a p e t p1 t1
end
load daten12a p e t p1 t1
[RDU,RDV,RDP,FU,FV] = feval(FF2,p,e,p1);
N = size(p,2) + size(p1,2);
RD = [RDU(1,:), RDV(1,:) + N,RDP(1)+2*N;
      RDU(2,:), RDV(2,:)    ,RDP(2)];
% ------------------------------
[U,V,P] = stokes1a(p,p1,t,t1,FU,FV,RD,Parmeter);
save daten12b U V P
disp(' Call bild08 ! ');
