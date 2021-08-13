function plate_domain(tout)
%PLATE_DOMAIN  (-1,1)x(1,1) square domain Q1 grid generator
%   plate_domain(tout)
%   input
%          tout       output on/off switch (optional)
% 
% grid data is saved to the file: square_plate.mat
%   IFISS  function: DJS; 14 August 2018; 29 July 2021
% Copyright (c) 2018 D.J. Silvester, P. Nadukandi
if nargin==0, tout = 0; end
fprintf('\nGrid generation for reference square domain\n')
nc=default('grid parameter: 3 for underlying 4x4 grid (default is 8x8)',4);
if nc<2, error('illegal parameter choice, try again.'), end
if nc>6,
stretch=default('refine into all corners? 1/0 (default is uniform)',0);
else, stretch=0;
end
%
n=2^(nc-1); % grid dimension
%
%% generate (x,y) coordinates of vertices
if stretch,
nn=n/2;
y=subint(-1,-5/nn,5/nn,1,nn-1,2,nn-1); whos
else
y=[-1:2/n:1]';
end
x=y; % y=y/10; <------ scale domain vertically 
%
%% generate element coordinates
nvtx=(n+1)*(n+1);
[X,Y]=meshgrid(x,y);
xx=reshape(X',nvtx,1);
yy=reshape(Y',nvtx,1);
xy=[xx(:),yy(:)];
%
kx = 1;
ky = 1;
el=0;
for j=1:n
   for i=1:n
      mref=(n+1)*(ky-1)+kx;
      el=el+1;
      nvv(1) = mref;
      nvv(2) = mref+1;
      nvv(3) = mref+n+2;
      nvv(4) = mref+n+1;
      ev(el,1:4)=nvv(1:4);
      kx = kx + 1;
   end
   ky = ky + 1;
   kx = 1;
end
%
%% generate boundary edge array
e1=[1:n]; ef1=ones(1,n);              % bottom edge
e2=[n:n:n*n]; ef2=2*ones(1,n);        % right edge
e3=[(n-1)*n+1:n*n]; ef3=3*ones(1,n);  % bottom edge
e4=[1:n:n*n]; ef4=4*ones(1,n);        % left edge
ebound=[e1',ef1';e2',ef2';e3',ef3';e4',ef4'];
%
% output
if (tout)
fprintf('\n xy data\n')
disp([xy]),
fprintf('\n element mapping data\n')
disp([ev]),
fprintf('\n boundary edge data\n')
disp([ebound])
end

% generate datafile
gohome, cd datafiles
save plate_grid.mat ev xy ebound
return
