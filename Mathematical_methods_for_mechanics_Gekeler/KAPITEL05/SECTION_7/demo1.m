function demo1
% Continuation after Allgower-Georg
% F(X) = 0, X n+1-vector, F(X) n-vector
% INPUT  'name'  Name of example
%        X       : f(X) = 0 start vector
% OUTPUT X       Solution path
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Lenght of path is ruled by parameter Maxit %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear, clc, format short, format compact
errorcode = 0; nr = 100; 
while ~ismember(nr,[1,2,3,4])
   nr = input(' Example No. (1/2/3/4)');
end;
switch nr
case 1, disp(' Some niveau lines ')
   Maxit = 2000; Tol = 1.0E-6; direction = 1;
   Parcont = [Maxit,Tol,direction];
   WEG1 = []; HOEHEN1 = [];
   for I = 1:4
      CASE1 = I
      Parmeter = [];
      X = [-1; 0.5 - 0.1*I];
      F0 = feval('bsp01',X,3,Parmeter);
      Parmeter = F0;
      [WEG,errorcode] = cont(@bsp01,X,Parcont,Parmeter);
      WEG1 = [WEG1;WEG];
      HOEHEN1 = [HOEHEN1;F0*ones(1,size(WEG,2))];
   end
   % ---------------------------------------------
   Maxit = 4000; Tol = 1.0E-6; direction = 1;
   Parcont = [Maxit,Tol,direction];
   WEG2 = []; HOEHEN2 = [];
   for I = 1:6
      CASE2 = I
      Parmeter = [];
      X = [1; 0.7 - 0.1*I];
      F0 = feval(@bsp01,X,3,Parmeter);
      Parmeter = F0;
      [WEG,errorcode] = cont(@bsp01,X,Parcont,Parmeter);
      WEG2 = [WEG2;WEG];
      HOEHEN2 = [HOEHEN2;F0*ones(1,size(WEG,2))];
   end
   save daten1 WEG1 HOEHEN1 WEG2 HOEHEN2;
   fig0520
case 2, disp(' Chemical reaction model after Kubizek ')
   % X(5) is path parameter; X(2) is plottet over X(5)
   X = zeros(5,1);
   Maxit = 1600; Tol = 1.0E-6; direction = - 1;
   Parcont = [Maxit,Tol,direction]; Parmeter = [];
   [WEG,errorcode] = cont(@bsp02,X,Parcont,Parmeter);
   save daten2 WEG;
   fig0521a
case 3, disp(' Example of Rheinboldt 86, S. 146 ')
   X = [15;-2;0];
   Maxit = 120; Tol = 1.0E-4; direction = 1;
   Parcont = [Maxit,Tol,direction]; Parmeter = [];
   [WEG,errorcode] = cont(@bsp03,X,Parcont,Parmeter);
   save daten3 WEG;
   fig0522_0523a
case 4, disp(' Example of Allgower/Georg ')
   % Norm of X(1:4) is plotted over X(5)
   X = zeros(5,1);
   Maxit = 1500; Tol = 1.0E-4; direction = 1;
   Parcont = [Maxit,Tol,direction]; Parmeter = [];
   [WEG,errorcode] = cont(@bsp04,X,Parcont,Parmeter);
   save daten4 WEG
   bild01
end
switch errorcode
case 0, disp(' Solution ')
case 1, disp(' Bad start vector ')
case 2, disp(' Bad conditon for start vector ')
case 3, disp('failure at minmal stepsize ')
case 4, disp(' maximal number of jacobian eval. exceeded ')
end
