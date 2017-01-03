%function [xtt] = SelectFeature(Ipi,thresh,boundary,spacing,winx,winy,Nmax,method)
% Selects feature points on the given image
%
% method= 0 -> old method of thresholding = max(q(:))/10
% method = 1 -> new method of thresholding = 10*mean(q(:))
%
%
%support routine for 'trackdemo.m' (help trackdemo)
%
%
%Contributors to this code include: Stefano Soatto, Jean-Yves Bouguet, Xiaolin Feng, 
%Hailin Jin, Paolo Favaro.
%Last updated 5/5/2003.
%
%DISTRIBUTED FREE FOR NON-COMMERCIAL USE
%Copyright (c) MASKS, 2003

function [xtt] = SelectFeature(Ipi)

% Required resolution (for the track, in pixel)
resolution = 0.03;
% Selection window sizes
winx = 1; winy = 1;
% saturation in Q for selection (win=1)
saturation = 7000;
% Tracking window sizes
wintx = 4; winty = 4;
% min spacing between 2 feats (in pixel).
spacing = 5;
% rejected pixels around the screen (selection)
boundary = 5;
% rejected pixels around the screen (tracking)
boundary_t = 1;
% max. selected features in selection
Nmax = 1000;
% Threshold of selection
thresh = 0.05;
% lower level in the pyramid
levelmin = 0;
% higher level in the pyramid
levelmax = 2;
% Thresh of ejection of a point
% through the track
ThreshQ = 0.1;
% Minimum space reserved for the
% feature storage
N_max_feat = 500;
% Set to 1 to take into consideration
% the saturation (used in selection
% and tracking)
method = 0;    

[nx, ny] = size(Ipi);

% compute the quality vector to select highly textured regions
q = GridQuality(Ipi,winx,winy);

%compute boundary mask
windboundary = zeros(nx,ny);
windboundary(boundary:nx-boundary+1,boundary:ny-boundary+1) = ...
   ones(nx-2*boundary+2,ny-2*boundary+2);
%mask out the boundary
q = q.*windboundary;

% threshold the quality vector
if method,
   %   maxq = max(max(q(find(q<saturation))));
   maxq = min(max(q(:)),saturation*min(winx,winy));
else
   maxq = max(max(q));
end;
thq = thresh*maxq;
% select local maxima
Q = (q > thq) & (LocalMax(q));
i = find(Q(:));
% recall that sort sorts in ascending 
% order, while we want the descending order
[Y,I] = sort(-q(i));
if (size(Y,1)>Nmax),
   Y = Y(1:Nmax);
   I = I(1:Nmax);
end;

% determine the columns and rows of each selected point
C = ceil(i(I)/nx);
R = rem(i(I)-1,nx)+1;
CC = C * ones(1,size(C,1));
RR = R * ones(1,size(R,1));
% we need to remove points that are too close to each other
% and therefore could lead to confuse one feature for another
D2 = (CC - CC').^2 + (RR-RR').^2;	%% matrix of square distances between features
D2_mod = tril(D2-spacing^2,-1);		%% take the lower-triangle

good_features = ~sum(D2_mod'<0);	%% if the sum is 0 it is a good feature
indexgood = find(good_features);

featR = R(indexgood);
featC = C(indexgood);

xtt = [featR,featC]';

% here we return the selected points
% is desired, the quality vector could
% also be returned
indxtt = (xtt(2,:)-1)*nx + xtt(1,:);
qxtt = q(indxtt);

return;
