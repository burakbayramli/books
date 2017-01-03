% RELELLIP  Computation of relative ellipse

%Kai Borre 10-25-96
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/26  $
% Modified 2012-APR-20 Kurt Feigl

% The following data are from Peter Richardus:
% Project Surveying, page 120

%    	xA      yA      xB       yB
S = [  4.10    0.17   -4.00    2.20;  % xA 
       0.17    4.20    2.10   -3.40;  % yA  
      -4.00    2.10    5.60   -1.20;  % xB   
       2.20   -3.40   -1.20    4.80]; % yB

% Compated to the book products xAyA, yAxA, xByB, yBxB 
% change sign because of changed x-direction  

% Entries S(1,3), S(1,4), S(2,3), S(2,4) and S(3,1), S3,2), 
% S(4,1), S(4,2) change sign because ? 
   
% Covariance matrices assuming A and B independent
CA = S(1:2,1:2)
CB = S(3:4,3:4)
%CI = CA + CB

% Covariance Matrix for B - A
% Modified 2012-APR-20 Kurt Feigl
CR = [S(1,1)+S(3,3)-2*S(3,1), S(1,2)-S(2,3)-S(1,4)+S(3,4); 
      S(1,2)-S(2,3)-S(1,4)+S(3,4), S(2,2)+S(4,4)-2*S(2,4)]

[aA, bA, alphaA] = ellaxes(S(1:2,1:2));
[aB, bB, alphaB] = ellaxes(S(3:4,3:4));
[aR, bR, alphaR] = ellaxes(CR);

% computing ellipse points
t = linspace(0,2*pi,50);
rotA = [cos(alphaA) -sin(alphaA); sin(alphaA) cos(alphaA)];
plA = [aA*sin(t); bA*cos(t)];
for t = 1:50,
   currentA = rotA*plA(:,t);
   curveA(1:2,t) = currentA;
end

t = linspace(0,2*pi,50);
rotB = [cos(alphaB) -sin(alphaB); sin(alphaB) cos(alphaB)];
plB = [aB*sin(t); bB*cos(t)];
for t = 1:50
   currentB = rotB*plB(:,t);
   curveB(1:2,t) = currentB;
end

t = linspace(0,2*pi,50);
rotR = [cos(alphaR) -sin(alphaR); sin(alphaR) cos(alphaR)];
plR = [aR*sin(t); bR*cos(t)];
for t = 1:50
   currentR = rotR*plR(:,t);
   curveR(1:2,t) = currentR;
end

% plotting the three ellipses
clf
hold on
axis equal
axis([-2,2,-2,2]);
plot(curveA(1,1:50)-1, curveA(2,1:50)-.5,'b-')
axesaA = rotA*[-aA  aA; 0 0];
axesbA = rotA*[0 0; -bA  bA];
plot(axesaA(1,:)-1, axesaA(2,:)-.5,'b--',...
                    axesbA(1,:)-1, axesbA(2,:)-.5,'b--')
plot([0 -1],[0 -.5],'b+-');

plot(curveB(1,1:50)+1, curveB(2,1:50)+.5,'r-')
axesaB = rotB*[-aB  aB; 0 0];
axesbB = rotB*[0 0; -bB  bB];
plot(axesaB(1,:)+1, axesaB(2,:)+.5,'r--',...
                    axesbB(1,:)+1, axesbB(2,:)+.5,'r--')
plot([0 +1],[0 +.5],'rv-');

% This is A - B
scale = 1; 
plot(curveR(1,1:50)*scale, curveR(2,1:50)*scale,'g-')
axesaR = rotR*[-aR  aR; 0 0]*scale;
axesbR = rotR*[0 0; -bR  bR]*scale;
plot(axesaR(1,:), axesaR(2,:),'g--',...
                  axesbR(1,:), axesbR(2,:),'g--')
plot(0,0,'g^-');
hold off
%%%%%%% end relellip.m  %%%%%%%%%%%%%%%%%%%%%
