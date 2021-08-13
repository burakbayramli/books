%UNPACK_BOUSSDATA sets up Boussinesq flow problem data 
% calls function find_elem.m
%   IFISS scriptfile: DJS; 3 May 2012.
% Copyright (c) 2012 D.J. Silvester, M.D. Mihajlovic.


%  Grid data for the finest level
%
npx=grid(1).npx;            %  nr of elements in x-dir 
npy=grid(1).npy;            %  nr of elements in y-dir 
ne=grid(1).ne;              %  total nr of elements 
mv2=grid(1).mv2;            %  Q2 local-to-global mapping
mp1=grid(1).mp1;            %  Q1 local-to-global mapping
mt2=grid(1).mt2;            %  Q2 local-to-global mapping
mt1=grid(1).mt1;            %  Q1 local-to-global mapping
xy2=grid(1).xy2;            %  (x,y) coordinates of Q2 grid nodes
xy1=grid(1).xy1;            %  (x,y) coordinates of Q1 grid nodes
x=grid(1).x;                %  coordinates of vertical grid lines
y=grid(1).y;                %  coordinates of horizontal grid lines
bnd_d=grid(1).bnd_d;        %  Q2 nodes lying on all 4 boundaries
bnd_dn2=grid(1).bnd_dn2;    %  Q2 nodes lying on 2 vertical boundaries
bnd_dn1=grid(1).bnd_dn1;    %  Q1 nodes lying on 2 vertical boundaries
xyv=grid(1).xyv;            %  (x,y) coordinates of velocity nodes
xyp=grid(1).xyp;            %  (x,y) coordinates of pressure nodes
xyt=grid(1).xyt;            %  (x,y) coordinates of temperature nodes
%
%  Assembled matrices for the finest level
%
Av=spmat(1).Av;             %  block 2x2 velocity Laplacian matrox
Qv=spmat(1).Qv;             %  block 2x2 velocity mass matrix
At=spmat(1).At;             %  temperature Laplacian matrix
Qt=spmat(1).Qt;             %  temperature mass matrix
B=spmat(1).B;               %  discrete divergence matrix
Bx=spmat(1).Bx;             %  x-component of discrete divergence matrix
By=spmat(1).By;             %  y-component of discrete divergence matrix
Qp=spmat(1).Qp;             %  pressure mass matrix
M=spmat(1).M;               %  N-S - temperature coupling mass matrix
BBx=spmat(1).BBx;           %  velocity divergence matrix (x-component)
BBy=spmat(1).BBy;           %  velocity divergence matrix (y-component)
%
%% setup history data points if output switch is on
if tout==2,
%global ttch uxh uyh dph Th
fprintf('History data : u_x(1,t),u_y(1,t),T(1,t), dp(t)=p(1,t)-p(4,t)\n'); 
fprintf('note that x-coordinates range from %8.3e to %8.3e\n',min(x),max(x)); 
fprintf('          y-coordinates range from %8.3e to %8.3e\n',min(y),max(y)); 
 xh1=default('   x-coordinate of the history point 1 ',0.181); 
 if(xh1<min(x) | xh1>max(x)),
      error('Oops: x1 is not in the domain'); end
 yh1=default('   y-coordinate                        ',7.370);
 if(yh1<min(y) | yh1>max(y)),
      error('Oops: y1 is not in the domain'); end
 xh4=default('   x-coordinate of the history point 4 ',0.819);
 if(xh4<min(x) | xh4>max(x)),
      error('Oops: x4 is not in the domain'); end
 yh4=default('   y-coordinate                        ',7.370);
 if(yh4<min(y) | yh4>max(y)),
      error('Oops: y4 is not in the domain'); end
%%
%% construct skew-symmetric point in the domain
xh2=L-xh1; yh2=H-yh1;
fprintf('skew-symmetric point: xh2 is %8.3e, yh2 is %8.3e\n',xh2,yh2); 
%  
%% Find the elements that contain the history points
[eh1,sh1,th1,eh2,sh2,th2]=find_elementref(xyv,mv2,xh1,yh1,xh2,yh2);
[eh1,sh1,th1,eh4,sh4,th4]=find_elementref(xyv,mv2,xh1,yh1,xh4,yh4);
[phih1,dphih1ds,dphih1dt]=shape(sh1,th1);    %  bilinear shape functions at (s1,t1)
[psih1,dpsih1ds,dpsih1dt]=qshape(sh1,th1);   %  biquadratic shape functions at (s1,t1) 
[phih2,dphih2ds,dphih2dt]=shape(sh2,th2);    %  bilinear shape functions at (s2,t2)
[psih2,dpsih2ds,dpsih2dt]=qshape(sh2,th2);   %  biquadratic shape functions at (s2,t2) 
[phih4,dphih4ds,dphih4dt]=shape(sh4,th4);    %  bilinear shape functions at (s4,t4)
%
%% define solution storage vectors
ttch=[];uxh=[];uyh=[];dph=[];Th=[];
end
