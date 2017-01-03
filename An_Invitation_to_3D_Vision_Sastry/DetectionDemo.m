% FEATURE DETECTION
% 
% Implements Harris' corner detector and its variation
% Support code for the book "An invitation to 3D vision"
% by Y. Ma, S. Soatto, J. Kosecka, S. Sastry.
%
%Contributors to this code include: Pietro Perona, Stefano Soatto, Andrea Mennucci, 
%Jean-Yves Bouguet, Xiaolin Feng, Hailin Jin, Paolo Favaro, Jana Kosecka, Yi Ma.
%Last updated 5/5/2003.
%
%DISTRIBUTED FREE FOR NON-COMMERCIAL USE
%Copyright (c) MASKS, 2003

clear all
close all

fprintf('\nDetection demo\n');
filename = 'boxes1.bmp';
fprintf('Loading file %s ... ',filename);
I=imread(filename);
I=mean(double(I),3);
fprintf('Done.\n');

fprintf('Selecting features ... ');
[x]=selectfeature(I);
fprintf('Done.\n\n');

image(I)
colormap gray(256)
titletext = sprintf('Feature selection on file %s',filename);
title(titletext);
axistext = sprintf('%i features detected',size(x,2));
xlabel(axistext);
drawnow
hold on
plot(x(2,:),x(1,:),'rs');
plot(x(2,:),x(1,:),'r.');
hold off

return
