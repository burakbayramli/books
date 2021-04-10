function [p,t] = initmesh(fd,fh,h0,bbox,pfix,varargin)
%% INITMESH initialize a triangulation
%
% Modifed from distmesh. Please refer to distmesh on the usage.
%
%   Example: (Rectangle with circular hole, refined at circle boundary)
%      fd=inline('ddiff(drectangle(p,-1,1,-1,1),dcircle(p,0,0,0.5))','p');
%      fh=inline('min(4*sqrt(sum(p.^2,2))-1,2)','p');
%      [p,t]=initmesh(fd,fh,0.05,[-1,-1;1,1],[-1,-1;-1,1;1,-1;1,1]);
%
%   Copyright (C) 2004-2006 Per-Olof Persson. See COPYRIGHT.TXT for details.

% parameters;
geps = 0.001*h0;

% 1. Create initial distribution in bounding box (equilateral triangles)
[x,y] = meshgrid(bbox(1,1):h0:bbox(2,1),bbox(1,2):h0*sqrt(3)/2:bbox(2,2));
x(2:2:end,:) = x(2:2:end,:)+h0/2;                % Shift even rows
p = [x(:),y(:)];                                 % List of node coordinates

% 2. Remove points outside the region, apply the rejection method
p = p(feval(fd,p,varargin{:}) < geps,:);        % Keep only d<0 points
r0 = 1./feval(fh,p,varargin{:}).^2;              % Probability to keep point
p = p(rand(size(p,1),1) < r0./max(r0),:);
p = [pfix; p + (rand(size(p,1),2)-2)*geps];  % Rejection method
% p = unique(p,'rows');
% p(1:size(pfix,1),:) = pfix;

% 3. Retriangulation by the Delaunay algorithm
t = delaunayn(p);                                % List of triangles
pmid = (p(t(:,1),:)+p(t(:,2),:)+p(t(:,3),:))/3;  % Compute centroids
t = t(feval(fd,pmid,varargin{:})<-geps,:);       % Keep interior triangles
