function [FF,FF_X,FF_Y,ecode] = rt_ff(X,Y,XX,YY)
% Formfunktionen fuer Rannacher-Turek-element
% X(I),Y(I) die vier Eckenkoordinatenpaare
% XX,YY Koordinaten eines bel. Punktes
% ecode = 1 bei falscher Orientierung
% FF Spalte der 4 Formfunktionen an der Stelle (XX,YY)
% FF_X Ableitung von FF nach XX an der Stelle (XX,YY)
% FF_Y Ableitung von FF nach YY an der Stelle (XX,YY)

ecode = 0;
A1 = (X(1)+X(2)+X(3)+X(4))/4;
A2 = (Y(1)+Y(2)+Y(3)+Y(4))/4;
B1 = (X(2)+X(3)-X(1)-X(4))/4;
B2 = (Y(2)+Y(3)-Y(1)-Y(4))/4;
C1 = (X(3)+X(4)-X(1)-X(2))/4;
C2 = (Y(3)+Y(4)-Y(1)-Y(2))/4;
DET = B1*C2-B2*C1;

XI    = (C2*(XX - A1) - C1*(YY - A2))/DET;
ETA   = (B1*(YY - A2) - B2*(XX - A1))/DET;
XI_X  =  C2/DET;  XI_Y  = -C1/DET;
ETA_X = -B2/DET;  ETA_Y =  B1/DET;
% Formfunktionen im speziellen lokalen Koordinatensystem

FF1 = ((1 - ETA)^2 - XI^2)/4;
FF2 = ((1 + XI)^2  - ETA^2)/4;
FF3 = ((1 + ETA)^2 - XI^2)/4;
FF4 = ((1 - XI)^2  - ETA^2)/4;

% Ableitungen nach XX und YY 

FF1_X = (-2*(1 - ETA)*ETA_X - 2*XI*XI_X)/4;
FF1_Y = (-2*(1 - ETA)*ETA_Y - 2*XI*XI_Y)/4;

FF2_X = (2*(1 + XI)*XI_X   - 2*ETA*ETA_X)/4;
FF2_Y = (2*(1 + XI)*XI_Y   - 2*ETA*ETA_Y)/4;

FF3_X = (2*(1 + ETA)*ETA_X - 2*XI*XI_X)/4;
FF3_Y = (2*(1 + ETA)*ETA_Y - 2*XI*XI_Y)/4;

FF4_X = (-2*(1 - XI)*XI_X   - 2*ETA*ETA_X)/4;
FF4_Y = (-2*(1 - XI)*XI_Y   - 2*ETA*ETA_Y)/4;

FF = [FF1;FF2;FF3;FF4];
FF_X = [FF1_X;FF2_X;FF3_X;FF4_X];
FF_Y = [FF1_Y;FF2_Y;FF3_Y;FF4_Y];


