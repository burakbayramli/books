function demo1b
% Master file for elliptic BVP
% linear triangular elements   
% Example: Stationary heat distribution after H.R. Schwarz
% FF1: File for first mesh (not used in MATLAB TOOLBOX)
% FF2: File for geometry in MATLAB Format 
% FF3: File for boundary conditions   

clear, clc, format short, format compact
disp(' No use of MATLAB TOOLBOX ')
OPTION = 100;
while ~ismember(OPTION,[1,2])
   OPTION = input(' Which mesh decomposition? (1/2) ');
end
%OPTION = 1;
% -- Parameter -------------------------------
RHO      = 0;     % Volume force density
Parmeter = RHO;

switch OPTION
case 1, disp(' Mesh without PDE TOOLBOX ')
   FF1 = 'bsp001a'; FF2 = 'bsp001g'; FF3 = 'bsp001h';  
   REFINE = 2;
   [p,e,t] = feval(FF1); % first mesh
   for J = 1:REFINE
      [p,e,t] = mesh01_t(FF2,p,e,t);
      p       = mesh10(p,e,t,4); % Jigglemesh
     % t       = mesh03(p,t,0);   % Lange Kanten durch kurze Ersetzen
   end
   clf, hold on
  % trimesh(t(1:3,:)',p(1,:),p(2,:),zeros(1,size(p,2)),'edgecolor','g'),
  % pause
case 2, disp(' Original mesh of Schwarz ')
   FF1 = 'bsp001b'; FF2 = 'bsp001g'; FF3 = 'bsp001h';
   [p,e,t] = feval(FF1,1);
end
save daten1 p e t
   bild00(p,e,t)
   pause
% ------------------------------------------
disp('Computation of solution ');
load daten1 p e t
[RD,RC,LASTEN] = feval(FF3,p,e,t);
LOESUNG = ellipt1(p,t,RD,RC,LASTEN,Parmeter);
save daten2 LOESUNG RD RC
bild01b
