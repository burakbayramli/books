function bouss_boxdomain(bsn)
%BOUSS_BOXDOMAIN rectangular cavity grid generator for Boussinesq
%   bouss_boxdomain;
%   input
%          bsn       temperature gradient switch
%            1       Bernard convection
%            2       lateral heating/cooling
%
%  Grid defining data is saved to two files: box_grid1h,  box_grid
%   IFISS scriptfile: DJS; 4 November 2013.
% Copyright (c) 2012 D.J. Silvester, M.D. Mihajlovic.

fprintf('\nGrid generation for Boussinesq box domain \n');
H=default('Input the domain height H (default 8)',8);
L=default('Input the domain width  L (default 1)',1);
gt=default('Grid type: uniform/stretched 1/2 (default stretched)', 2);
gh=1;
switch(gh)
   case(1)   %  single grid
      if(H>=L)   %  vertical cavity
         npx=default('Input the number of elements in x direction',0);
         if(npx<=0)
            error('Illegal choice, try again.');
         end
      else       %  horizontal cavity
         npy=default('Input the number of elements in y direction',0);
         if(npy<=0)
            error('Illegal choice, try again.');
         end
      end   
      nh=1;   %  number of hierarchy levels
   case(2)    %  grid hierarchy (needed for GMG)
      fprintf('Number of grid hierarchy levels.\n');
      nh=default('Input the number of grid hierarchy levels',0);
      if(nh<=0)
         error('Illegal choice, try again.');
      end
      if(H>=L)   %  vertical cavity
         npx=2^nh;   %  number of elements in x direction
      else       %  horizontal cavity
         npy=2^nh;  
      end    
end
if(H>=L)   %  horizontal cavity
   fprintf('Number of elements in y-direction. \n');
   str=default('Input the multiplication factor in y-direction (default H/L)',H/L);
else       %  vertical cavity
   fprintf('Number of elements in x-direction. \n');
   str=default('Input the multiplication factor in x-direction (default L/H)',L/H);
end
if((gh==1 & str<1) | (gh==2 & floor(str)~=str))
   error('Illegal stretch parameter, try again.');
end
if(H>=L)   %  vertical cavity
   npy=floor(str*npx);
else       %  horizontal cavity
   npx=floor(str*npy);      
end   
if(gt==2)
   sx=default('Input the stretch factor for x direction (default 2)',2);
   sy=default('Input the stretch factor for y direction (default 1)',1);
end
net=default('Choose node enumeration direction x/y : 1/2 (default is y)',2);
%%
%% set Bousinesq problem type 
hty=bsn;  
switch(hty)
   case(1)   %  Rayleigh-Bernard
      fprintf('Type of BC for the temperature on vertical walls:');
   case(2)   %  differentially heated from side
      fprintf('Type of BC for the temperature on horizontal walls:');
end      
fprintf('\n   1   Adiabatic (Neumann, default)');
fprintf('\n   2   Perfectly conducting (Dirichlet)');
hbbc=default('',1);
tic;
nx=2*npx;             %  number of lines of nodes in x direction
ny=2*npy;             %  number of lines of nodes in y direction 
ne=npx*npy;           %  number of elements
%  
%% Compute (x,y) coordinates of vertices
%
for lh=1:nh      %  loop over all grid levels
   if(gh==2 & lh>1)  %  coarser levels
      ny=ny/2.0;   %  cut nr of grid lines by 2
      nx=nx/2.0;   %  cut nr of grid lines by 2
      npy=npy/2.0; %  cut nr of elements by 2
      npx=npx/2.0; %  cut nr of elements by 2
      ne=npx*npy;  %  number of elements
%  clear previous level variables      
      if(gt==2)    %  non-uniform grid
         xc=x(1:4:length(x));   %  store coarse element vortices
         yc=y(1:4:length(y));   %  store coarse element vortices
      end
      clear x; clear y;
      clear yy; clear xx; clear X; clear Y;      
      clear xy2; clear xy1;    
      clear x1; clear y1; clear X1; clear Y1;   
      clear mv2; clear mp1; clear mt2; clear mt1;
      clear bnd_d; clear bnd_dn2; clear bnd_dn1;
      clear bnd_dv2; clear bnd_dnt1; clear bnd_dnt2;
   end
   switch(gt)
      case(1)    %  uniformly stretched grid 
%  y-direction
         hy=H/ny; yy=[0:hy:H]; y=yy; 
         hymin=2*hy; hymax=2*hy;
%  x-direction
         hx=L/nx; xx=[0:hx:L]; x=xx;
         hxmin=2*hx; hxmax=2*hx;
      case(2)    %  non-uniformly stretched grid (Davis-Bansch approach)
         if(lh==1)   %  finest grid 
            hxav=L/npx;      %  aver. elem. width x-dir
            hyav=H/npy;      %  aver. elem. width y-dir
            hhx(1:npx)=0.0; hhy(1:npy)=0.0;  %  elem width and len distrib.
            for i=1:npx
               hhx(i)=1.0/(hxav*(abs(npx+1-2*i))^sx+2);
            end
            for i=1:npy
               hhy(i)=1.0/(hyav*(abs(npy+1-2*i))^sy+2);
            end
            shhx=sum(hhx); shhy=sum(hhy);
            xx(1:nx+1)=0.0; j=1; hxmax=0.0; hxmin=inf;
            for i=1:npx
               xx(j+2)=xx(j)+hhx(i)/shhx*L;
               xx(j+1)=xx(j)+abs(xx(j+2)-xx(j))/2.0;
               if(abs(xx(j+2)-xx(j))<hxmin) 
                  hxmin=abs(xx(j+2)-xx(j));
               end
               if(abs(xx(j+2)-xx(j))>hxmax)
                  hxmax=abs(xx(j+2)-xx(j));
               end    
               j=j+2;
            end 
            xx(nx+1)=L;
            yy(1:ny+1)=0.0; j=1; hymax=0.0; hymin=inf;
            for i=1:npy
               yy(j+2)=yy(j)+hhy(i)/shhy*H;
               yy(j+1)=yy(j)+abs(yy(j+2)-yy(j))/2.0;
               if(abs(yy(j+2)-yy(j))<hymin) 
                  hymin=abs(yy(j+2)-yy(j));
               end
               if(abs(yy(j+2)-yy(j))>hymax)
                  hymax=abs(yy(j+2)-yy(j));
               end    
               j=j+2;
            end 
            yy(ny+1)=H;
         else   %  coarser levels 
            j=1; xx(1:2*length(xc)-1)=0.0;
            hxmax=0.0; hxmin=inf;
            for i=1:length(xc)   
               xx(j)=xc(i);   %  vertices
               if(i<length(xc))
                  xx(j+1)=xx(j)+abs(xc(i+1)-xc(i))/2.0;  %  mid-points
                  if(abs(xc(i+1)-xc(i))<hxmin)
                     hxmin=abs(xc(i+1)-xc(i));
                  end
                  if(abs(xc(i+1)-xc(i))>hxmax)
                     hxmax=abs(xc(i+1)-xc(i));
                  end
               end
               j=j+2;
            end
            j=1; yy(1:2*length(yc)-1)=0.0;
            hymax=0.0; hymin=inf;
            for i=1:length(yc)
               yy(j)=yc(i);   %  vertices
               if(i<length(yc))
                  yy(j+1)=yy(j)+abs(yc(i+1)-yc(i))/2.0;  %  mid-points
                  if(abs(yc(i+1)-yc(i))<hymin)
                     hymin=abs(yc(i+1)-yc(i));
                  end
                  if(abs(yc(i+1)-yc(i))>hymax)
                     hymax=abs(yc(i+1)-yc(i));
                  end
               end
               j=j+2;
            end
         end
         x=xx; y=yy;
   end          
%
%% Grid statistics
%
   fprintf('\nGrid statistics (level %2i):\n',lh);
   fprintf('   Number of elements: %5i (%3i x %3i)\n',ne,npx,npy);
   fprintf('   hxmin=%6.4f    hxmax=%6.4f\n',hxmin,hxmax);
   fprintf('   hymin=%6.4f    hymax=%6.4f\n',hymin,hymax);
%
%% Compute Q1 and Q2 element coordinates and node numbers
%
   nvtx=(nx+1)*(ny+1);
   [X,Y]=meshgrid(x,y);
   switch(net)   %  node ordering type
      case(1)    %  lexicographical in x-dir 
         xx=reshape(X',nvtx,1);  
         yy=reshape(Y',nvtx,1);
      case(2)    %  lexicographical in y-dir
         xx=reshape(X,nvtx,1);  
         yy=reshape(Y,nvtx,1); 
   end
   xy2=[xx(:),yy(:)];  
   nvtx1=(nx/2+1)*(ny/2+1);
   for i=1:2:nx+1
      x1((i+1)/2)=x(i); 
   end
   for i=1:2:ny+1
      y1((i+1)/2)=y(i); 
   end
   [X1,Y1]=meshgrid(x1,y1);
   switch(net)   %  node ordering type
      case(1)    %  lexicographical in x-dir
         xx1=reshape(X1',nvtx1,1);
         yy1=reshape(Y1',nvtx1,1);
      case(2)    %  lexicographical in y-dir
         xx1=reshape(X1,nvtx1,1);
         yy1=reshape(Y1,nvtx1,1);
   end
   xy1=[xx1(:),yy1(:)];
   kx=1;
   ky=1;
   nel=0;
   for j=1:npy
      for i=1:npx
         switch(net)   %  node ordering style 
            case(1)    %  lexicographical in x-dir
               mref=(nx+1)*(ky-1)+kx;   %  reference Q2 node
               pref=(npx+1)*(j-1)+i;    %  reference Q1 node
               nel=nel+1;
               nvv(1)=mref;
               nvv(2)=mref+2;
               nvv(3)=mref+2*nx+4;
               nvv(4)=mref+2*nx+2;
               nvv(5)=mref+1;
               nvv(6)=mref+nx+3; 
               nvv(7)=mref+2*nx+3; 
               nvv(8)=mref+nx+1;
               nvv(9)=mref+nx+2; 
               npp(1)=pref;
               npp(2)=pref+1;
               npp(3)=pref+npx+2;
               npp(4)=pref+npx+1;
            case(2)   %  lexicographical in y-dir
               mref=(ny+1)*(kx-1)+ky;   %  reference Q2 node
               pref=(npy+1)*(i-1)+j;    %  reference Q1 node
               nel=nel+1;
               nvv(1)=mref;
               nvv(2)=mref+2*ny+2;
               nvv(3)=mref+2*ny+4;
               nvv(4)=mref+2;
               nvv(5)=mref+ny+1;
               nvv(6)=mref+2*ny+3;
               nvv(7)=mref+ny+3;
               nvv(8)=mref+1;
               nvv(9)=mref+ny+2;
               npp(1)=pref;
               npp(2)=pref+npy+1;
               npp(3)=pref+npy+2;
               npp(4)=pref+1;
         end
         mv2(nel,1:9)=nvv(1:9);
         mp1(nel,1:4)=npp(1:4);
         mt2(nel,1:9)=nvv(1:9);
         mt1(nel,1:4)=npp(1:4);
         kx=kx+2;
      end
      ky=ky+2; 
      kx=1;
   end
%  
%%  Compute boundary vertices and edges
%
%  boundary edge 0<=x<=L, y=0
   k12=find(xy2(:,2)==0); 
   k11=find(xy1(:,2)==0);
   e1=[]; c1=[]; d1=[]; 
   switch(hty)
      case(1)   %  differentially heated from bottom/top
         for k=1:ne
            if(any(mv2(k,5)==k12))
               e1=[e1,k]; 
            end 
            if(any(mt2(k,5)==k12))
               c1=[c1,k]; 
            end    
            if(any(mt1(k,1)==k11))
               d1=[d1,k]; 
            end  
         end   
      case(2)   %  differentially heated from side
         for k=1:ne
            if(any(mv2(k,5)==k12))
               e1=[e1,k]; 
            end  
            if(hbbc==2)   %  perfectly conducting walls
               if(any(mt2(k,5)==k12))
                  c1=[c1,k]; 
               end    
               if(any(mt1(k,1)==k11))
                  d1=[d1,k]; 
               end    
            end
         end
   end
   ef1=ones(size(e1)); cf1=ones(size(d1)); df1=ones(size(d1));
%  boundary edge x=L, 0<=y<=H
   k22=find(xy2(:,1)==L & xy2(:,2)<=H & xy2(:,2)>=0);
   k21=find(xy1(:,1)==L & xy1(:,2)<=H & xy1(:,2)>=0);
   e2=[]; c2=[]; d2=[];
   switch(hty)
      case(1)   %  differentially heated from bottom/top
         for k=1:ne
            if(any(mv2(k,6)==k22))
               e2=[e2,k]; 
            end
            if(hbbc==2)   %  perfectly conducting walls
               if(any(mt2(k,6)==k22))
                  c2=[c2,k]; 
               end
               if(any(mt1(k,2)==k21))
                  d2=[d2,k]; 
               end
            end   
         end
      case(2)   %  differentially heated from side
         for k=1:ne 
            if(any(mv2(k,6)==k22))
               e2=[e2,k]; 
            end   
            if(any(mt2(k,6)==k22))
               c2=[c2,k]; 
            end
            if(any(mt1(k,2)==k21))
               d2=[d2,k]; 
            end
         end   
   end
   ef2=2*ones(size(e2)); cf2=2*ones(size(c2)); df2=2*ones(size(d2));
%  boundary edge 0<=x<=L, y=H
   k32=find(xy2(:,2)==H);
   k31=find(xy1(:,2)==H);
   e3=[]; c3=[]; d3=[];
   switch(hty)
      case(1)
   %  differentially heated from bottom/top
         for k=1:ne
            if(any(mv2(k,7)==k32))
               e3=[e3,k]; 
            end
            if(any(mt2(k,7)==k32)) 
               c3=[c3,k]; 
            end    
            if(any(mt1(k,3)==k31))
               d3=[d3,k]; 
            end 
         end
      case(2)    %  differentially heated from side
         for k=1:ne
            if(any(mv2(k,7)==k32))
               e3=[e3,k]; 
            end
            if(hbbc==2)   %  perfectly conducting walls
               if(any(mt2(k,7)==k32)) 
                  c3=[c3,k]; 
               end    
               if(any(mt1(k,3)==k31))
                  d3=[d3,k]; 
               end    
            end
         end
   end      
   ef3=3*ones(size(e3)); cf3=3*ones(size(c3)); df3=3*ones(size(d3));
%  boundary edge x=0, 0<=y<=H
   k42=find(xy2(:,1)==0 & xy2(:,2)<=H & xy2(:,2)>=0);
   k41=find(xy1(:,1)==0 & xy1(:,2)<=H & xy1(:,2)>=0);
   e4=[]; c4=[]; d4=[];
   switch(hty)
      case(1)   %  differentially heated from bottom/top
         for k=1:ne
            if(any(mv2(k,8)==k42))
               e4=[e4,k]; 
            end 
            if(hbbc==2)   %  perfectly conducting walls
               if(any(mt2(k,8)==k42))
                  c4=[c4,k]; 
               end
               if(any(mt1(k,4)==k41))
                  d4=[d4,k]; 
               end
            end
 
         end
      case(2)   %  differentially heated from side
         for k=1:ne
            if(any(mv2(k,8)==k42))
               e4=[e4,k]; 
            end   
            if(any(mt2(k,8)==k42))
               c4=[c4,k]; 
            end
            if(any(mt1(k,4)==k41))
               d4=[d4,k]; 
            end
         end 
   end      
   ef4=4*ones(size(e4)); cf4=4*ones(size(c4)); df4=4*ones(size(d4)); 
%
   bnd_d=unique([k12;k22;k32;k42]);  %  Q2 bnd nodes on all 4 boundaries
   switch(hty)
      case(1)   %  differentially heated from bottom/top
         switch(hbbc)
            case(1)   %  adiabatic vertical walls
               bnd_dn2=unique([k12;k32]);           %  Q2 bnd nodes on 2 horis. boundaries
               bnd_dn1=unique([k11;k31]);           %  Q1 bnd nodes on 2 horis. boundaries
            case(2)   %  perfectly conducting vertical walls
               bnd_dn2=unique([k12;k22;k32;k42]);   %  Q2 bnd nodes on all 4 boundaries
               bnd_dn1=unique([k11;k21;k31;k41]);   %  Q1 bnd nodes on all 4 boundaries
         end
      case(2)   %  differentially heated from side
         switch(hbbc)
            case(1)   %  adiabatic horizontal walls
               bnd_dn2=unique([k22;k42]);           %  Q2 bnd nodes on 2 vert. boundaries
               bnd_dn1=unique([k21;k41]);           %  Q1 bnd nodes on 2 vert. boundaries
            case(2)   %  perfectly conducting horizontal walls
               bnd_dn2=unique([k12;k22;k32;k42]);   %  Q2 bnd nodes on all 4 boundaries
	           bnd_dn1=unique([k11;k21;k31;k41]);   %  Q1 bnd nodes on all 4 boundaries
         end	
   end 
   bnd_dv2=[e1',ef1';e2',ef2';...
            e3',ef3';e4',ef4'];    %  Q2 elements lying on all 4 boundaries
   switch(hty)
      case(1)   %  differentially heated from bottom/top  
         switch(hbbc)
            case(1)   %  adiabatic horizontal walls
               bnd_dnt2=[c1',cf1';c3',cf3'];   %  Q2 elements lying on 2 horis. boundaries
               bnd_dnt1=[d1',df1';d3',df3'];   %  Q1 elements lying on 2 horis. boundaries
            case(2)   %  perfectly conducting horisontal walls
               bnd_dnt2=[c1',cf1';c2',cf2';...
	                 c3',cf3';c4',cf4'];   %  Q2 elements lying on all 4 boundaries
               bnd_dnt1=[d1',df1';d2',df2';...
	                 d3',df3';d4',df4'];   %  Q1 elements lying on all 4 boundaries
         end
      case(2)   %  differentially heated from side 
         switch(hbbc)
            case(1)   %  adiabatic horizontal walls	   
               bnd_dnt2=[c2',cf2';c4',cf4'];   %  Q2 elements lying on 2 vert. boundaries  
               bnd_dnt1=[d2',df2';d4',df4'];   %  Q1 elements lying on 2 vert. boundaries
            case(2)   %  perfectly conducting horisontal walls
               bnd_dnt2=[c1',cf1';c2',cf2';...
	                 c3',cf3';c4',cf4'];   %  Q2 elements lying on all 4 boundaries
               bnd_dnt1=[d1',df1';d2',df2';...
	                 d3',df3';d4',df4'];   %  Q1 elements lying on all 4 boundaries
         end
   end     		   		   	 
%

%
%% Store the grid data into the grid structure
%
   grid(lh).npx=npx;
   grid(lh).npy=npy;
   grid(lh).ne=ne;
   grid(lh).mv2=mv2;     
   grid(lh).mp1=mp1;
   grid(lh).mt2=mt2;
   grid(lh).mt1=mt1;
   grid(lh).xy2=xy2;
   grid(lh).xy1=xy1;
   grid(lh).x=x;
   grid(lh).y=y;
   grid(lh).bnd_d=bnd_d;
   grid(lh).bnd_dn2=bnd_dn2;
   grid(lh).bnd_dn1=bnd_dn1;
   grid(lh).bnd_dv2=bnd_dv2;
   grid(lh).bnd_dnt1=bnd_dnt1;
   grid(lh).bnd_dnt2=bnd_dnt2;
end    %  loop over all grid levels 
etoc=toc; 
fprintf('Grid partitioning done in %8.3f seconds\n',etoc);
%
%%  Save the grid hierarchy to a relevant file
%
gohome; cd datafiles
mv=mv2; xy=xy2; grid_type=gt; bound=bnd_dn2; mbound=bnd_dnt2;
save box_grid.mat mv xy bound mbound H L nh grid_type x y 
% Boussinesq case
save box_grid1h.mat grid str H L gh nh hbbc hty
fprintf('Grid data saved in box_grid1h.mat.\n')
return
