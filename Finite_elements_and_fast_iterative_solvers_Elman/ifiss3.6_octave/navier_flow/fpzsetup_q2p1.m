
function [av,fv] = fpzsetup_q2p1(xy,xyp,mv,ee,flowsol,viscosity,domain)
%FPZSETUP_Q2P1 modified P1 PCD matrix 
%   [Ap,Fp] = fpzsetup_q2p1(xy,xyp,mv,ee,flowsol,viscosity,domain);
%   input
%          xy         Q2 nodal coordinate vector 
%          xyp        Q0 element coordinate vector
%          mv         Q2 element mapping matrix
%          ee         element edge index vector
%          flowsol    Q1-Q1 or Q1-P0 flow solution
%          viscosity  viscosity parameter
%          domain     domain index
%   output
%          Ap         P1 scalar diffusion matrix
%          Fp         P1 scalar convection-diffusion matrix
%
%   NonUniform grid version: see ESW pp.350-352.
%   IFISS function: corrected DJS; 14 January 2010.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
x=xy(:,1); y=xy(:,2);
xp=xyp(:,1); yp=xyp(:,2);
nvtx=length(x); nu=2*nvtx; np=length(xp);
nel=length(mv(:,1));
usol=flowsol(1:nvtx); vsol=flowsol(nvtx+1:2*nvtx); 
fprintf('setting up modified Q2-P1 pressure preconditioning matrices... \n')
hx=zeros(nel,1); hy=zeros(nel,1);
fprintf('NonUniform grids are fine.\n')
%
% initialise global scalar matrices
a = sparse(np,np);
f = sparse(np,np);
%
%
%% remove zero indices corresponding to boundary edges
ppk=[1:nel]'; onecol=ones(nel,1); 
% find boundary edges
iie=ee==0;
eex=ee + ( ee==0).* [ppk,ppk,ppk,ppk];
%
tic,
%% check that interior edges have correct orientation
for elem=1:nel;
   xlocal=xyp(eex(elem,:),1); ylocal=xyp(eex(elem,:),2);
   xg=xyp(elem,1); yg=xyp(elem,2);
   ilist=find(xlocal==xg); jlist=find(ylocal==yg);
   [xm,i4]=min(xlocal(jlist)); e4=jlist(i4);
   [xm,i2]=max(xlocal(jlist)); e2=jlist(i2);
   [ym,i1]=min(ylocal(ilist)); e1=ilist(i1);
   [ym,i3]=max(ylocal(ilist)); e3=ilist(i3);
   if sum(iie(elem,:))==2, %% two boundary edge elements
   %fprintf('element %i has two boundary edges \n',elem)
      bedge=iie(elem,:);
      active=jlist(~bedge(jlist));
      if active==e4, e2=mod4(e4+2); else, e4=mod4(e2+2); end,
      active=ilist(~bedge(ilist));  
      if active==e1, e3=mod4(e1+2); else, e1=mod4(e3+2); end,
   end
%%
%fprintf('element %i reordered via mapping [%i,%i,%i,%i] \n',elem, e1,e2,e3,e4)
   eex(elem,:)=eex(elem,[e1,e2,e3,e4]);
end
etime=toc;
%fprintf('%g seconds needed to reorientate edges \n', etime)
%
%% form the pressure Laplacian and convection-diffusion matrix
%
% inner loop over elements    
for ivtx = 1:9
   xl_v(:,ivtx) = x(mv(:,ivtx));
   yl_v(:,ivtx) = y(mv(:,ivtx)); 
   xsl_v(:,ivtx) = usol(mv(:,ivtx));
   ysl_v(:,ivtx) = vsol(mv(:,ivtx));
end
%
%% compute local mesh sizes
hx(:)=xl_v(:,2)-xl_v(:,1); 
hy(:)=yl_v(:,3)-yl_v(:,2); 
hxohy= hx(:)./hy(:); hyohx= hy(:)./hx(:);
%hxohy= onecol; hyohx= onecol;  %%% Testing
        
% evaluate normal velocity on each edge in turn 
%% nx= 0, ny=-1  
wind_y = -ysl_v(:,5)*0.5.*hx(:);
f=f + sparse(1:nel,eex(:,1), wind_y,nel,nel);
f=f + sparse(1:nel,1:nel,   -wind_y,nel,nel);
%f=f + sparse(1:nel,1:nel,    wind_y,nel,nel);
a=a + sparse(1:nel,eex(:,1),-hxohy,nel,nel);
a=a + sparse(1:nel,1:nel,    hxohy,nel,nel);
%
%% nx= 1, ny=0  
wind_x = xsl_v(:,6)*0.5.*hy(:);
f=f + sparse(1:nel,eex(:,2),  wind_x,nel,nel);
f=f + sparse(1:nel,1:nel,    -wind_x,nel,nel);
%f=f + sparse(1:nel,1:nel,    wind_x,nel,nel);
a=a + sparse(1:nel,eex(:,2),-hyohx,nel,nel);
a=a + sparse(1:nel,1:nel,    hyohx,nel,nel);
%
%% nx= 0, ny=1  
wind_y = ysl_v(:,7)*0.5.*hx(:);
f=f + sparse(1:nel,eex(:,3), wind_y,nel,nel);
f=f + sparse(1:nel,1:nel,   -wind_y,nel,nel);
%f=f + sparse(1:nel,1:nel,    wind_y,nel,nel);
a=a + sparse(1:nel,eex(:,3),-hxohy,nel,nel);
a=a + sparse(1:nel,1:nel,    hxohy,nel,nel);
%
%% nx= -1, ny=0  
wind_x = -xsl_v(:,8)*0.5.*hy(:);
f=f + sparse(1:nel,eex(:,4), wind_x,nel,nel);
f=f + sparse(1:nel,1:nel,   -wind_x,nel,nel);
%f=f + sparse(1:nel,1:nel,    wind_x,nel,nel);
a=a + sparse(1:nel,eex(:,4),-hyohx,nel,nel);
a=a + sparse(1:nel,1:nel,    hyohx,nel,nel);
%
%
f = viscosity*a + f;
%%
%%% Robin b.c. for Fp at inflow 
xmin = min(xy(:,1));
if domain == 3 || domain == 10 || domain == 4,  % step, channel, obstacle
    fprintf('Robin pressure on inflow boundary\n')
%   nu p_x = w1 p For Fp at inflow
    boundp = find(xyp(:,1)>xmin & xyp(:,1)<xmin+hx); %find(xyp(:,1)==xmin+hx/2); 
    uxbd=specific_flow(xyp(boundp,1)-hx(boundp)/2,xyp(boundp,2));
    fprintf('check inflow boundary velocity\n')
    disp([xyp(boundp,1)-hx(boundp)/2, xyp(boundp,2), uxbd])
    for edge=1:length(uxbd),
       f(boundp(edge),boundp(edge)) = f(boundp(edge),boundp(edge)) + ...
           uxbd(edge)*hy(boundp(edge));
    end
% Dirichlet b.c. for Ap and Fp at outflow: add effect of "ghost points" to the 
% right of the boundary back to the diagonal   
   xmax = max(xy(:,1));
   bound_out_p = find(xyp(:,1)>xmax-hx & xyp(:,1)<xmax); %find(xyp(:,1)==xmax-hx/2);
   a_east = sparse(1:nel,eex(:,2),-hyohx,nel,nel);
   a(bound_out_p,bound_out_p) = a(bound_out_p,bound_out_p) ...
                                  - a_east(bound_out_p,eex(bound_out_p,2));
   f_east = viscosity*a_east + sparse(1:nel,eex(:,2),  wind_x,nel,nel);
   f(bound_out_p,bound_out_p) = f(bound_out_p,bound_out_p) ...
                                  - f_east(bound_out_p,eex(bound_out_p,2));
else
   fprintf('singular matrices are used here\n');
end
   
%%
%% create vector version of discrete operators
npp=3*np; ss=speye(np,np);
av=sparse(npp,npp);
av(1:3:npp,1:3:npp)=a;
av(2:3:npp,2:3:npp)=ss;
av(3:3:npp,3:3:npp)=ss;
fv=sparse(npp,npp);
fv(1:3:npp,1:3:npp)=f;
fv(2:3:npp,2:3:npp)=viscosity*ss;
fv(3:3:npp,3:3:npp)=viscosity*ss;
return
%--------------------------------------------------------------
function nn=mod4(n)
if n<5, nn=n; else nn=mod(n,4); end
