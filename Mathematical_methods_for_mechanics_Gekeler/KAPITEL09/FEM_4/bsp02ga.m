function [x,y] = bsp07g(bs,s)
% wie BSP02G.M aber skaliert
% convection in a closed compartment
nbs=4;
if nargin == 0,x = nbs; return, end
% d = [start par. value; end par. value;
%       left hand region; right hand region]
d = [...
% 1 2 3 4
  0 0 0 0 ; 1 1 1 1 ; 1 1 1 1 ; 0 0 0 0 ];
% -- Randsegmente  ----------------
bs1 = bs(:)';
if find(bs1<1 | bs1>nbs),
  error('Non existent boundary segment number')
end

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
% convection in closed box, Randsegmente
% A = [X-Werte; Y-Werte];
switch segnr
case 1, A = [0  10; 0  0];
case 2, A = [10  10; 0  15];
case 3, A = [10  0; 15  15];
case 4, A = [0  0; 15  0];
end
A = A/100; %%Einzige Aenderung gegenueber BSP02G.M
N = size(A,2); L = 0; AUX = zeros(1,N); P = AUX;
for I = 1:N-1
    % ungefaehre Laenge der Segmente
    AUX(I) = sqrt((A(1,I+1) - A(1,I))^2  + (A(2,I+1) - A(2,I))^2);
end
L = sum(AUX); AUX = AUX/L;
for I = 2:N
    P(I) = P(I-1) + AUX(I-1);
end
X = A(1,:); Y = A(2,:);

