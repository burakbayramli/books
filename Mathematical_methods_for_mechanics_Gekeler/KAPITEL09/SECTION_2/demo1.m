function demo1
% Example 9.2: Stationary heat distribution after H.R. Schwarz
% FF1: Geometry file
% FF2: File for boundary conditions   

clear, clc, format short, format compact
% -- Parameter -------------------------------
RHO = 0;     % Volume force density
Parmeter = RHO;
FF1 = @bsp01g; FF2 = @bsp01h;
[p,e,t] = feval(FF1); % first mesh
[RD,RC,LASTEN] = feval(FF2,e);
SOLUTION = ellipt(p,t,RD,RC,LASTEN,Parmeter);
save daten p e t SOLUTION RD RC
fig0904
