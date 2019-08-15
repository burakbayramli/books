function P = pressure(FF3,p,e,p1,t,t1,U,V,NU,pcond,FF4)
% post processor after Sohn
% calculates pressure from velocity field
% for Taylor-Hood elements
% see options below

if nargin == 9, pcond = 1; FF4 = []; end
if nargin == 10, FF4 = []; end
b = 10;

N1 = size(p,2); N2 = size(p1,2); N = N1 + N2;
S = sparse(N,N); P1 = S;  P2 = S; 
Q1 = S; Q2 = S; Q3 = S; Q4 = S; B = zeros(N,1); FF = B;
for I = 1:size(t,2)
   J = t(1:3,I); K = t1(:,I); L = [J;K];
   X = p(1,J); Y = p(2,J); UK = U(L); VK = V(L);
   X1 = [X,p1(1,K-N1)]; Y1 = [Y,p1(2,K-N1)];
   [SE,PP1,PP2,AU,BU,CV,DV,BE_Q,ME] = pressure_aux(X,Y,UK,VK);
   S(L,L)  = S(L,L)  + SE;        % stiffness matrix for U resp. V
   P1(L,L) = P1(L,L)  + PP1;        
   P2(L,L) = P2(L,L)  + PP2;        
   Q1(L,L) = Q1(L,L)  + AU;
   Q2(L,L) = Q2(L,L)  + BU;
   Q3(L,L) = Q3(L,L)  + CV;
   Q4(L,L) = Q4(L,L)  + DV;
   B(L)    = B(L)  + BE_Q;
   if ~isempty(FF4)
      div_f = feval(FF4,X1,Y1,b);
      FF(L) = FF(L) + ME*div_f; 
   end
end
RSIDE = NU*(P1*U + P2*V) - (Q1*U + Q2*V + Q3*U + Q4*V) - FF;

b = 10;
switch pcond
case 1 % One value of pressure specified  
   [RDU,RDV,RDP] = feval(FF3,p,e,p1,b);
   J  = RDP(1);
   S(J,:) = 0; S(:,J) = 0; S(J,J) = 1; 
   RSIDE(J) = RDP(2);
   P = S\RSIDE; 

case 2 % implicit condition for pressure
   S = [S,B];
   S = [S;[B.',0]];
   RSIDE = [RSIDE;0];
   P = S\RSIDE;
   P = P(1:end-1);
   %spy(S), pause
   
% Test for Example 5:   
case 3 % One value of pressure specified  
   J  = size(p,2); % bad result
   S(J,:) = 0; S(:,J) = 0; S(J,J) = 1; 
   X = p(1,J); Y = p(2,J);
   AUX = 2*pi*cos(pi*X)*sin(pi*Y);
   RSIDE(J) = AUX;
   P = S\RSIDE; 
case 4 % One value of pressure specified  
   J  = size(p,2); % bad result
   BB = zeros(N,1); BB(J) = 1;
   S = [S,BB];
   S = [S;[BB.',0]];
   X = p(1,J); Y = p(2,J);
   AUX = 2*pi*cos(pi*X)*sin(pi*Y);
   RSIDE = [RSIDE;AUX];
   P = S\RSIDE; 

end