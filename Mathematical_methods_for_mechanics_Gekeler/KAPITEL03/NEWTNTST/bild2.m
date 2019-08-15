function bild2
% Figure for Exsample 2 ------------------
load daten H X
clf
T   = 0:H:1;
Phi = 0:pi/12:2*pi;
XX  = ones(length(Phi),1)*T;
YY  = cos(Phi(:))*X(1,:);
ZZ  = sin(Phi(:))*X(1,:);
mesh(XX,YY,ZZ), view(-45,50), axis square
title(' Rotational surface ')
