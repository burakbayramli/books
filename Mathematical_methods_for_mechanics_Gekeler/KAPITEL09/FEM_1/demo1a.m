function demo1a
% Master file for elliptic BVP
% linear triangular elements, mesh adaption
% Example 1: Example with exact soution to compare
% Example 2: Stationary heat distribution after H.R. Schwarz
% FF1: File for first mesh (not used in MATLAB TOOLBOX)
% FF2: File for geometry in MATLAB Format 
% FF3: File for boundary conditions   

clear, clc, format short, format compact
disp(' Uses MATLAB TOOLBOX ')
EXAMPLE = 100; GRAFIK = 100;
while ~ismember(EXAMPLE,[1,2])
   EXAMPLE = input(' Which example? (1/2) ');
end
%EXAMPLE = 2;
while ~ismember(GRAFIK,[0,1])
   GRAFIK  = input(' Grafik on/off (1/0) ');
end
%EXAMPLE = 2;

% -- Parameter -------------------------------
REFINE   = 4; % number of initial uniform mesh refinements
REFINE2  = 3; % Number of adaptions
RHO      = 0;     % Volume force density
Parmeter = RHO;
switch EXAMPLE
case 1, FF2 = 'bsp001g'; FF3 = 'bsp001h'; % Example
case 2, FF2 = 'bsp001g'; FF3 = 'bsp001h_tst'; % Example
end
[p,e,t] =  initmesh(FF2,'hmax',inf);
p = jigglemesh(p,e,t,'Opt','minimum');
for J = 1:REFINE
   [p,e,t] = refinemesh(FF2,p,e,t,'longest');
   p       = jigglemesh(p,e,t,'Opt','minimum');
end
% -- Order boundary segments ----
% (makes boundary data file more secure) --
LL = max(e(5,:)); AUX = [];
for I = 1:LL
   J = find(e(5,:) == I); EE = e(:,J);
   [U,K] = sort(EE(3,:)) ; EE = EE(:,K);
   AUX = [AUX,EE];
end
e = AUX;

save daten1 p e t EXAMPLE
if GRAFIK == 1, bild00(p,e,t), pause, end
% ------------------------------------------
disp('Computation of solution ');
load daten1 p e t EXAMPLE
[RD,RC,LOADS] = feval(FF3,p,e,t);
SOLUTION = ellipt1(p,t,RD,RC,LOADS,Parmeter);
save daten2 SOLUTION RD RC
%bild01a,
%pause
% -----------------------------------------------
for I = 1:REFINE2
   disp(' Adaption ');
   STEP = I
   load daten1 p e t EXAMPLE
   load daten2 SOLUTION RD RC
   [p,e,t] = myadapt(FF2,p,e,t,SOLUTION,LOADS,GRAFIK);
   p = jigglemesh(p,e,t,'Opt','minimum');
   [RD,RC,LOADS] = feval(FF3,p,e,t);
   SOLUTION = ellipt1(p,t,RD,RC,LOADS,Parmeter);
   save daten1 p e t EXAMPLE
   save daten2 SOLUTION RD RC
   if GRAFIK == 1, pause, end
end
bild01a
