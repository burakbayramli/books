function demo1a
% Masterfile for shallow water problem
% Island in a bay; following H.Ninomiya/K.Onishi
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% flow at coast following Ninomija %%%%
%%% WITHOUT MATLAB PDE TOOLBOX %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% p = (p1;p2;p3) : (X,Y)-Coordinates, water depth
% V = (V1;V2;V3) : V1 = U, V2 = V Velocity;
% V3 = Z         : Tidal elevation
% FF1        : Geometrie-File
% FF2        : File der Randbedingungen 
clc, clear, format compact
disp(' Beispiel 1: Island in a Bay ')
%%% Parametereingabe %%%%%%%%%%%%%%%%%%%%%
FF1 = 'bsp01'; FF2 = 'bsp01g'; FF3 = 'bsp01h';

REFINE = 1; % Number of uniform mesh refinements
JIGGLE = 1;
% -----------------------------
   [p,e,t,depth] = feval(FF1); % first mesh
   pold = p; eold = e;
   for J = 1:REFINE
      disp(' Refinemesh ')
      [p,e,t] = mesh01_t(FF2,p,e,t);
      if JIGGLE == 1
      %   p       = mesh10(p,e,t,4); % Jigglemesh
      %   t       = mesh03(p,t,0);   % replace long edges
      end  
   end
X = pold(1,:); Y = pold(2,:);   
xlin = linspace(min(X),max(X),30);
ylin = linspace(min(Y),max(Y),30);
[X1,Y1] = meshgrid(xlin,ylin);
W1 = griddata(X,Y,depth,X1,Y1,'cubic');

X2 = p(1,:); Y2 = p(2,:);
W2 = interp2(X1,Y1,W1,X2,Y2,'cubic',0);


%return
bild00(p,e,t)
pause
%N = find_number(p,t)
%DD = W2(N)

figure = 1
switch figure
case 1
clf
xlin    = linspace(min(X2),max(X2),40);
ylin    = linspace(min(Y2),max(Y2),40);
[X3,Y3] = meshgrid(xlin,ylin);
   clf
%   W2 = -W2*1000;
   W4 = griddata(X2,Y2,W2,X3,Y3,'cubic');
   contour(X3,Y3,W4,10), hold on

   %mesh(X3,Y3,W4), hold on
   axis equal
   %view(30,70)
case 2   
   clf
     I = find(eold(5,:) == 1); LI = length(I);
   A = eold(1,I);
   I = find(eold(5,:) == 2); LI = length(I);
   A = [A,eold(1,I),eold(2,I(LI))];
   X  = pold(1,A); Y = pold(2,A);% Z = -p(3,A);
   plot(X,Y,'r','linewidth',2,'erasemode','none'), hold on
   I = find(eold(5,:) == 3); LI = length(I);
   B = [eold(1,I),eold(2,I(LI))];
   X  = pold(1,B); Y = pold(2,B); %Z = -p(3,B);
   plot(X,Y,'r','linewidth',2,'erasemode','none'), hold on
   fill(X,Y,'g','erasemode','none'), hold on
   axis equal
   xlin    = linspace(min(X2),max(X2),40);
   ylin    = linspace(min(Y2),max(Y2),40);
   [X3,Y3] = meshgrid(xlin,ylin);
%   W2 = -W2*1000;
    W4 = griddata(X2,Y2,W2,X3,Y3,'cubic');
   contour(X3,Y3,W4,10), hold on
   axis([0 10000 0 16000])
   axis equal tight
end
