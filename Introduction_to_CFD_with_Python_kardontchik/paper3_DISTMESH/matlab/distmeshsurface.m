function [p,t]=distmeshsurface(fd,fh,h0,bbox,varargin)
%DISTMESHSURFACE 3-D Surface Mesh Generator using Distance Functions.
%   [P,T]=DISTMESHSURFACE(FD,FH,H0,BBOX,FPARAMS)
%
%   NOTE:
%      Doesn't work too well for nonuniform size functions... need a
%      better initial grid or density control. Also no support for
%      fixed points, again because of the initial grid.
%
%      P:         Node positions (Nx2)
%      T:         Triangle indices (NTx3)
%      FD:        Distance function d(x,y)
%      FH:        Scaled edge length function h(x,y)
%      H0:        Initial edge length
%      BBOX:      Bounding box [xmin,ymin; xmax,ymax]
%      FPARAMS:   Additional parameters passed to FD and FH
%
%   Example: (Uniform Mesh on Unit Sphere)
%      fd=@(p) dsphere(p,0,0,0,1);
%      [p,t]=distmeshsurface(fd,@huniform,0.2,1.1*[-1,-1,-1;1,1,1]);
%
%   Example: (Graded Mesh on Unit Sphere)
%      fd=@(p) dsphere(p,0,0,0,1);
%      fh=@(p) 0.05+0.5*dsphere(p,0,0,1,0);
%      [p,t]=distmeshsurface(fd,fh,0.15,1.1*[-1,-1,-1;1,1,1]);
%
%   Example: (Uniform Mesh on Torus)
%      fd=@(p) (sum(p.^2,2)+.8^2-.2^2).^2-4*.8^2*(p(:,1).^2+p(:,2).^2);
%      [p,t]=distmeshsurface(fd,@huniform,0.1,[-1.1,-1.1,-.25;1.1,1.1,.25]);
%
%   Example: (Uniform Mesh on Ellipsoid)
%      fd=@(p) p(:,1).^2/4+p(:,2).^2/1+p(:,3).^2/1.5^2-1;
%      [p,t]=distmeshsurface(fd,@huniform,0.2,[-2.1,-1.1,-1.6; 2.1,1.1,1.6]);

%   Copyright (C) 2004-2012 Per-Olof Persson. See COPYRIGHT.TXT for details.

dptol=1e-4; ttol=.1; Fscale=1.2; deltat=.2; deps=sqrt(eps)*h0;

% 1. Create initial distribution in bounding box (isosurface from grid)
[x,y,z]=ndgrid(bbox(1,1):h0:bbox(2,1),bbox(1,2):h0:bbox(2,2),bbox(1,3):h0:bbox(2,3));
pv=isosurface(x,y,z,reshape(fd([x(:),y(:),z(:)],varargin{:}),size(x)),0);
p=pv.vertices;
t=pv.faces;

% Connectivities (for trisurfupd)
[t2t,t2n]=mkt2t(t);
t2t=int32(t2t-1)'; t2n=int8(t2n-1)';

N=size(p,1);                                         % Number of points N
pold=inf;                                            % For first iteration
while 1
  p0=p;
  % 3. Retriangulation
  if max(sqrt(sum((p-pold).^2,2))/h0)>ttol           % Any large movement?
    pold=p;                                          % Save current positions
    [t,t2t,t2n]=trisurfupd(int32(t-1)',t2t,t2n,p');  % Update triangles
    t=double(t+1)';
    pmid=(p(t(:,1),:)+p(t(:,2),:)+p(t(:,3),:))/3;    % Compute centroids
    % 4. Describe each bar by a unique pair of nodes
    bars=[t(:,[1,2]);t(:,[1,3]);t(:,[2,3])];         % Interior bars duplicated
    bars=unique(sort(bars,2),'rows');                % Bars as node pairs
    % 5. Graphical output of the current mesh
    clf,patch('faces',t,'vertices',p,'facecol',[.8,.9,1],'edgecol','k');
    axis equal;axis off;view(3);cameramenu;drawnow
  end

  % 6. Move mesh points based on bar lengths L and forces F
  barvec=p(bars(:,1),:)-p(bars(:,2),:);              % List of bar vectors
  L=sqrt(sum(barvec.^2,2));                          % L = Bar lengths
  hbars=feval(fh,(p(bars(:,1),:)+p(bars(:,2),:))/2,varargin{:});
  L0=hbars*Fscale*sqrt(sum(L.^2)/sum(hbars.^2));     % L0 = Desired lengths
  F=max(L0-L,0);                                     % Bar forces (scalars)
  Fvec=F./L*[1,1,1].*barvec;                         % Bar forces (x,y,z components)
  Ftot=full(sparse(bars(:,[1,1,1,2,2,2]),ones(size(F))*[1,2,3,1,2,3],[Fvec,-Fvec],N,3));
  p=p+deltat*Ftot;                                   % Update node positions

  % 7. Bring all points back to the boundary
  d=feval(fd,p,varargin{:});
  dgradx=(feval(fd,[p(:,1)+deps,p(:,2),p(:,3)],varargin{:})-d)/deps; % Numerical
  dgrady=(feval(fd,[p(:,1),p(:,2)+deps,p(:,3)],varargin{:})-d)/deps; % gradient
  dgradz=(feval(fd,[p(:,1),p(:,2),p(:,3)+deps],varargin{:})-d)/deps; %
  dgrad2=dgradx.^2+dgrady.^2+dgradz.^2;
  p=p-[d.*dgradx./dgrad2,d.*dgrady./dgrad2,d.*dgradz./dgrad2];  % Project back to boundary

  % 8. Termination criterion: All nodes move less than dptol (scaled)
  if max(sqrt(sum((p-p0).^2,2))/h0)<dptol, break; end
end
