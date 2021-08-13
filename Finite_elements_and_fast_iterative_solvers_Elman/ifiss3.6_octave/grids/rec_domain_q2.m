function [xy,mv] = rec_domain_q2(x1,x2,y1,y2,h,xyo)
%REC_DOMAIN_Q2 generate Q2 subdivision of a rectangular domain 
%                complementing grid points on a neigboring domain
%   [xy,mv] = rec_domain_q2(x1,x2,y1,y2,h,xyo)
%   input
%          x1, x2     limits of x
%          y1, y2     limits of y
%          h          mesh size
%          xyo        current list of coordinates from neighboring domain
%   output
%          xy         coordinate vector
%          mv         q2 element mapping matrix
%
%   IFISS function: HCE; 24 December 2009.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage;
% Code written by M. Wu 2009

xmin = min(x1,x2);
xmax = max(x1,x2);
ymin = min(y1,y2);
ymax = max(y1,y2);

x = [xmin:h:xmax]';
y = [ymin:h:ymax]';
nx = length(x)-1;
ny = length(y)-1;
nxyo = size(xyo,1);
nvtx=(nx+1)*(ny+1);
[X,Y]=meshgrid(x,y);
xx=reshape(X',nvtx,1);
yy=reshape(Y',nvtx,1);
xy=[xx(:),yy(:)];
xyg = [1:nvtx]' + nxyo;

if nxyo~=0
   xyn = [];
   for i = 1:nvtx
       ix = find(xyo(:,1) == xy(i,1) & xyo(:,2) == xy(i,2));
       if size(ix,1)~=0
          xyg(i) = ix;
          if i<nvtx
             xyg((i+1):nvtx) = xyg((i+1):nvtx) - 1;
          end
       end
       if xyg(i)>nxyo
           xyn = [xyn;xy(i,:)];
       end
   end
   xy = xyn;
end
   
%
kx = 1;
ky = 1;
mel=0;
for j=1:ny/2
   for i=1:nx/2
      mref=(nx+1)*(ky-1)+kx;
      mel=mel+1;
      nvv(1) = xyg(mref);
      nvv(2) = xyg(mref+2);
      nvv(3) = xyg(mref+2*nx+4);
      nvv(4) = xyg(mref+2*nx+2);
      nvv(5) = xyg(mref+1);
      nvv(6) = xyg(mref+nx+3); 
      nvv(7) = xyg(mref+2*nx+3); 
      nvv(8)=  xyg(mref+nx+1);
      nvv(9)=  xyg(mref+nx+2); 
      mv(mel,1:9)=nvv(1:9);
      kx = kx + 2;
   end
   ky = ky + 2; 
   kx = 1;
end
return
