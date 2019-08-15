function [x,y] = bsp02g(bs,s)
% Gabelschluessel fuer PDE TOOLBOX, Geometriedaten
%
%   NE=bsp04g gives the number of boundary segment
%
%   D=bsp04g(bs) gives a matrix with one column for each
%   boundary segment specified in BS.
%   Row 1 contains the start parameter value.
%   Row 2 contains the end parameter value.
%   Row 3 contains the number of the left hand region.
%   Row 4 contains the number of the right hand region.
%
%   [X,Y]=bsp04g(BS,S) gives coordinates of boundary points.
%   BS specifies the boundary segments and S the
%   corresponding parameter values. BS may be a scalar.

% -- number of boundary segments ---------
nbs=38;
if nargin == 0,x = nbs; return, end

% d = [start par. value; end par. value;
%       left hand region; right hand region]

d1 = [0 0 0 0; 1 1 1 1; 1 1 1 1; 0 0 0 2];
d2 = [0 0 0 0; 1 1 1 1; 2 2 2 2; 0 1 0 3];
d3 = [0 0 0 0; 1 1 1 1; 3 3 3 3; 0 2 0 4];
d4 = [0 0 0 0; 1 1 1 1; 4 4 4 4; 0 3 0 5];
d5 = [0 0 0 0 0 0; 1 1 1 1 1 1; 5 5 5 5 5 5; 0 4 0 6 0 8];
d6 = [0 0 0 0 ; 1 1 1 1 ; 6 6 6 6 ; 0 5 0 7 ];
d7 = [0 0 0 0 ; 1 1 1 1; 7 7 7 7; 0 0 6 0];
d8 = [0 0 0 0 ; 1 1 1 1 ; 8 8 8 8 ; 0 5 0 9 ];
d9 = [0 0 0 0 ; 1 1 1 1; 9 9 9 9; 0 8 0 0];

d = [d1,d2,d3,d4,d5,d6,d7,d8,d9];
% -- Randsegmente  ----------------
bs1 = bs(:)';
if find(bs1<1 | bs1>nbs),
  error('Non existent boundary segment number'), end

if nargin == 1, x = d(:,bs1); return, end
x = zeros(size(s)); y = zeros(size(s));
[m,n] = size(bs);
if m == 1 & n == 1,
   bs = bs*ones(size(s)); % expand bs
elseif m ~= size(s,1) | n ~= size(s,2),
   error('bs must be scalar or of same size as s');
end

if ~isempty(s)
   for K = 1:nbs
      ii = find(bs == K); % boundary segment K
      if length(ii)
        [X,Y,P] = bsp02f(K);
         x(ii) = interp1(P,X,s(ii),'linear');
         y(ii) = interp1(P,Y,s(ii),'linear');
      end
   end
end

function [X,Y,P] = bsp02f(segnr)
% spanner, Randsegmente
% A = [X-Werte; Y-Werte];
[YI,ZI] = bsp02_aux;

switch segnr
% Patch1
case 1,  A = [17.2, 21  ; 1,   0];
case 2,  A = [21, 21.6  ; 0, 2.1];
case 3,  A = [21.6,17.8 ;2.1,3.1];
case 4,  A = [17.8, 17.2; 3.1, 1];
% Patch 2
case 5,  A = [13.4, 17.2;  2,  1];
case 6,  A = [17.2, 17.8;  1,3.1];
case 7,  A = [17.8,   14;3.1,4.1];
case 8,  A = [14,   13.4; 4.1, 2];
% Patch 3
case 9,  A = [9.6,  13.4; 3,   2];
case 10, A = [13.4,  14; 2, 4.1];
case 11, A = [14,  10.2;4.1,5.1];
case 12, A = [10.2, 9.6; 5.1, 3];
% Patch 4
case 13, A = [6.5,  9.6; 3.8, 3];
case 14, A = [9.6,10.2; 3, 5.1];
case 15, A = [10.2,  7; 5.1, 6];
case 16, A = [7,   6.5; 6,  3.8];
% Patch 5
case 17, A = [5.35,6.5;3.9,3.8];
case 18, A = [6.5,7;3.8,6];
case 19,  A = [7,5.75;6,6.6];
case 20,  A = [5.75,3.5;6.6,6.5];
case 21, A = [3.5,3.5,3.5;6.5,5.8,5.1];
case 22, A = [3.5, 5.35;5.1,3.9];
% Patch 6
case 23,  A = [2.8, 3.5;7,6.5];
case 24,  A = [3.5,5.75;6.5,6.6];
case 25, A = [ZI([1,3,5,7,9,11,13,15,17,19,21],1)';
              ZI([1,3,5,7,9,11,13,15,17,19,21],2)'];
case 26, A = [3.9, 2.8; 8.5,7];

% Patch 7
case 27,  A = [1,1.8;7,7];
case 28,  A = [1.8,2.8; 7,7];
case 29,  A = [2.8,3.9;7,8.5];
case 30, A = [ZI([21,26,31,36,43,51],1)';
              ZI([21,26,31,36,43,51],2)'];
% Patch 8
case 31,  A = [YI([21,23,25,27,29,31,33,35,37,41],1)';
               YI([21,23,25,27,29,31,33,35,37,41],2)'];
case 32,  A = [5.35,3.5; 3.9,5.1];
case 33, A = [3.5,2.8;5.1,4.6];
case 34, A = [2.8,2.5;4.6,3.3];
% Patch 9
case 35,  A = [YI([1,5,9,13,17,21],1)';YI([1,5,9,13,17,21],2)'];
case 36, A = [2.5,2.8;3.3,4.6];
case 37, A = [2.8,1.8;4.6,4.6];
case 38, A = [1.8,1;4.6,4.6];

end
N = size(A,2); L = 0; AUX = zeros(1,N); P = AUX;
for I = 1:N-1
    % ungefaehre Laenge der Segmente
    AUX(I) = sqrt((A(1,I+1) - A(1,I))^2  + (A(2,I+1) - A(2,I))^2);
end
L = sum(AUX); AUX = AUX/L;
for I = 2:N, P(I) = P(I-1) + AUX(I-1); end
X = A(1,:); Y = A(2,:);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [YI,ZI] = bsp02_aux
% Berechnet Splines der beiden krummen Randkurven
% von Spanner
%clc, clf
% -- Ecken -----------
%plot(0,0,'w*'), hold on
%plot(6,6,'w*'), hold on
%axis equal tight, axis manual, grid on
Z = [5.75,   5,  3.9,  2.5, 1.45, 1;
     6.6, 7.85,  8.5,  8.3, 7.65, 7]';
Y = [1, 1.4,2.5,3.9,5.35; 4.6,4.0,3.3,3.1,3.9]';

N = size(Y,1);
X = [1:N]';
XI = linspace(1,N,10*(N-1)+1)';
YI = interp1(X,Y,XI,'spline');
LYI = size(YI,1);
%plot(YI(:,1),YI(:,2),'k'),hold on
%plot(Y(:,1),Y(:,2),'k*'),hold on
%plot(YI(1:11,1),YI(1:11,2),'r','linewidth',2),hold on
%plot(YI(11,1),YI(11,2),'ko'), hold on
%plot(YI(11:21,1),YI(11:21,2),'r','linewidth',2),hold on
%plot(YI(21,1),YI(21,2),'ko'), hold on

N = size(Z,1);
X = [1:N]';
XI = linspace(1,N,10*(N-1)+1)';
ZI = interp1(X,Z,XI,'spline');
LZI = size(ZI,1);

