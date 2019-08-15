function demo2
% Continuation after Rheinboldt86
% F(X) = 0, X n+1-vector, F(X) n-vector
% INPUT  'name'  Name of example
%        X       F(X) = 0 start vector
% OUTPUT X       Solution path

clear, clc, format short, format compact
nr = 100; 
while ~ismember(nr,[2,3,4])
   nr   = input(' Example no. (2/3/4) ');
end;
switch nr
case 2, disp(' Chemical reaction model after Kubizek ')
   maxit    = 190;                % Max. Schrittzahl in PITCON1.M
   tol      = 1.0E-3;           % Toleranz
   hmin     = 1.0E-5;           % kleinste Schrittweite
   dir      = 1;              % Anfangsrichtung
   Parcont  = [maxit,tol,hmin]; % Parameter fuer CONT1.M
   Parmeter = [];               % Parameter fuer Beispiel
   % -- START ------------------------
   X0       = zeros(5,1);            % Startvektor mit F(X) = 0
   E0       = [1,0,0,0,0];           % Vektor mit E*T > 0 fuer erste Tangente
   H0       = 0.0001; STEP = [H0, H0]; % Erste Schrittweiten
   [WEG,TANG,SS,ecode] = pitcon5(@bsp02,X0,E0,H0,tol,dir,Parmeter);
   if ecode == 0
      [WEG1,TANG1,STEP1,errorcode] = pitcon1(@bsp02,WEG,TANG,STEP,Parcont,Parmeter);
      save daten2 WEG1 TANG1 STEP1
   end
   fig0521b
case 3
   maxit    = 200;                % Max. Schrittzahl in PITCON1.M
   tol      = 1.0E-3;           % Toleranz
   hmin     = 0.01;             % kleinste Schrittweite
   dir      = - 1;              % Anfangsrichtung
   Parcont  = [maxit,tol,hmin]; % Parameter fuer CONT1.M
   Parmeter = [];               % Parameter fuer Beispiel
   % -- START ------------------------
   X0       = [15;-2;0];            % Startvektor mit F(X) = 0
   E0       = [1,0,0];              % Vektor mit E*T > 0 fuer erste Tangente
   H0       = 0.2; STEP = [H0, H0]; % Erste Schrittweiten
   [WEG,TANG,SS,ecode] = pitcon5(@bsp03,X0,E0,H0,tol,dir,Parmeter);
   if ecode == 0
      [WEG1,TANG1,STEP1,errorcode] = pitcon1(@bsp03,WEG,TANG,STEP,Parcont,Parmeter);
      save daten3 WEG1 TANG1 STEP1
   end
   fig0522_0523b
case 4
   maxit    = 150;                % Max. Schrittzahl in PITCON1.M
   tol      = 1.0E-3;           % Toleranz
   hmin     = 0.001;             % kleinste Schrittweite
   dir      = - 1;              % Anfangsrichtung
   Parcont  = [maxit,tol,hmin]; % Parameter fuer CONT1.M
   Parmeter = [];               % Parameter fuer Beispiel
   % -- START ------------------------
   X0       = zeros(5,1);            % Startvektor mit F(X) = 0
   E0       = [1,0,0,0,0];           % Vektor mit E*T > 0 fuer erste Tangente
   H0       = 0.1; STEP = [H0, H0]; % Erste Schrittweiten
   dir      = 1;
   [WEG,TANG,SS,ecode] = pitcon5(@bsp04,X0,E0,H0,tol,dir,Parmeter);
   if ecode == 0
      [WEG,TANG,STEP,errorcode] = pitcon1(@bsp04,WEG,TANG,STEP,Parcont,Parmeter);
      save daten4 WEG
   end
   bild01
end
%switch errorcode
%case 0, disp(' Solution ')
%case 1, disp(' Bad start vector ')
%case 2, disp(' Bad conditon for start vector ')
%case 3, disp('failure at minmal stepsize ')
%case 4, disp(' maximal number of jacobian eval. exceeded ')
%end

