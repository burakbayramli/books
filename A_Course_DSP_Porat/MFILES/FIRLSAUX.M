function [V,Ad] = firlsaux(typ,theta,deltap,deltas,thetai);
% Synopsis: [V,Ad] = firlsaux(typ,theta,deltap,deltas,thetai).
% An auxiliary function for FIRLS.
% Input parameters: see firls.m
% Output parameters:
% V, Ad: variables needed in firls.m

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

ind1 = find(thetai < theta(1));
ind3 = find(thetai > theta(length(theta)));
if (typ == 'p' | typ == 's'),
ind2 = find(thetai > theta(2) & thetai < theta(3)); end
V = zeros(1,length(thetai)); Ad = zeros(1,length(thetai));
if (typ == 'l'),
   Ad(ind1) = ones(1,length(ind1));
   Ad(ind3) = zeros(1,length(ind3));
   V(ind1) = (1/deltap)*ones(1,length(ind1));
   V(ind3) = (1/deltas)*ones(1,length(ind3));
elseif (typ == 'h'),
   Ad(ind1) = zeros(1,length(ind1));
   Ad(ind3) = ones(1,length(ind3));
   V(ind1) = (1/deltas)*ones(1,length(ind1));
   V(ind3) = (1/deltap)*ones(1,length(ind3));
elseif (typ == 'p'),
   Ad(ind1) = zeros(1,length(ind1));
   Ad(ind2) = ones(1,length(ind2));
   Ad(ind3) = zeros(1,length(ind3));
   V(ind1) = (1/deltas(1))*ones(1,length(ind1));
   V(ind2) = (1/deltap(1))*ones(1,length(ind2));
   V(ind3) = (1/deltas(2))*ones(1,length(ind3));
elseif (typ == 's'),
   Ad(ind1) = ones(1,length(ind1));
   Ad(ind2) = zeros(1,length(ind2));
   Ad(ind3) = ones(1,length(ind3));
   V(ind1) = (1/deltap(1))*ones(1,length(ind1));
   V(ind2) = (1/deltas(1))*ones(1,length(ind2));
   V(ind3) = (1/deltap(2))*ones(1,length(ind3));
end

