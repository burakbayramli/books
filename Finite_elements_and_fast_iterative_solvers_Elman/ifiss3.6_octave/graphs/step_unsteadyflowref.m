function step_unsteadyflowref(qmethod,ev,sol,tt,A,By,Bx,G,xy,xyp,x,y,bound,snaptime,symm)
%STEP_UNSTEADYFLOWREF plots step flow data at snapshot times
%   step_unsteadyflowref(qmethod,mv,U,time,A,By,Bx,G,xy,xyp,x,y,bound,snaptime,symm);
%   input
%          qmethod    mixed method 
%          ev         mv/ev  Q2/Q1 element mapping matrix
%          U          flow solution vector
%          time       snapshot time vector
%          A          vector diffusion matrix
%          By         velocity  y-derivative matrix    
%          Bx         velocity x-derivative matrix    
%          G          veclocity mass matrix
%          xy         velocity nodal coordinate vector  
%          xyp        pressure nodal coordinate vector  
%          x          vector of x-axis interpolation points
%          y          vector of y-axis interpolation points
%          bound      boundary vertex vector
%          snaptime   vector of snapshot time step levels
%          symm       symmetric domain 0/1 switch (optional)
%
% calls function xxstreambc.m to set boundary values
%   IFISS function: DJS; 20 September 2016.
% Copyright (c) 2014 D.J. Silvester, H.C. Elman, A. Ramage
if nargin < 15, symm = 0; end
fprintf('\n   Plotting flow field snapshots ... ')
L=max(x); nstep=length(snaptime);
if nstep>9, error('Too many snapshots!'), end
nvtx=length(xy); nu=2*nvtx; np=length(xyp);
[LG,UG]=lu(G(1:nvtx,1:nvtx)); 
Asv=A(1:nvtx,1:nvtx); fzero=zeros(nvtx,1);
[Abc,fzero]=streambc(Asv,fzero,xy,bound);
[LA,UA]=lu(Abc); 
%
fprintf('\n   step   time    mean_vorticity    min_phi  max_phi\n')
%
% ------------------ loop over snapshots
for k=1:nstep
kk=snaptime(k); ttk=tt(kk);
% compute derived quantites
u=sol(:,kk);
ux=u(1:nvtx); uy=u(nvtx+1:nu);  utotal=sqrt(ux.*ux+ uy.*uy);
fsv=-[By,-Bx]*u;
omega=UG\(LG\fsv);
f=[By,-Bx]*u;
[fsv]=xxstreambc(Asv,f,xy,bound,ttk);   
phi=UA\(LA\fsv);  
if qmethod > 1, wev = vorticity_q2(xy,ev,omega,0);
else, wev = vorticity_q1(xy,ev,omega,0); end
%
% interpolate to a cartesian product mesh
[X,Y]=meshgrid(x,y);
%
% plot stream function
figure(101)
indx=100*nstep +10 +k;
subplot(indx)
ax = [min(x)-.1 max(x)+.1 min(y)-.1 max(y)+.1];
xysol = griddata(xy(:,1),xy(:,2),phi,X,Y);
   if symm, % symmetric step
   [II,JJ]=find(X<0 & Y<-0.5); xysol(II,JJ)=nan;
   [II,JJ]=find(X<0 & Y>0.5); xysol(II,JJ)=nan;
   else
   [II,JJ]=find(X<0 & Y<0); xysol(II,JJ)=nan;
   end
maxphi=max(max(xysol)); minphi=min(min(xysol));
fprintf('  %4i  %7.3f  %11.3e   %12.5f  %9.3e\n', ...
            kk, ttk,  sum(wev), min(phi), max(phi));
vneg=[minphi:-minphi/6:0];
vpos=[maxphi/20:maxphi/20:19*maxphi/20];
vpospos=[79*maxphi/80: maxphi/320:maxphi];
   if L<=5 %default domain
   contour(X,Y,xysol,[vneg,vpos]) 
   axis equal
   else
   contour(X,Y,xysol,[vneg,vpos,vpospos])
  %contour(X,Y,xysol,[vneg,vpospos])
   axis equal, axx=ax(1:4); axx(2)=min(30,axx(2));
   axis(axx(1:4)); 
   end
    title(['Stationary streamlines: time = ',num2str(ttk,'%5.2f')],'FontSize',12), 
if symm, stepsym, else, stepx, end, axis('off')
%
% plot vorticity
figure(102)
subplot(indx)
xysol = griddata(xy(:,1),xy(:,2),omega,X,Y);
solheight = max(max(xysol))-min(min(xysol));
ax5 = min(min(xysol))-.1*solheight;
ax6 = max(max(xysol))+.1*solheight;
ax = [min(x)-.1 max(x)+.1 min(y)-.1 max(y)+.1 ax5 ax6];
   if symm, % symmetric step
   [II,JJ]=find(X<0 & Y<-0.5); xysol(II,JJ)=nan;
   [II,JJ]=find(X<0 & Y>0.5); xysol(II,JJ)=nan;
   else
   [II,JJ]=find(X<0 & Y<0); xysol(II,JJ)=nan;
   end
maxphi=max(max(xysol)); minphi=min(min(xysol));
vneg=[minphi:-minphi/12:0];
vpos=[maxphi/20:maxphi/20:19*maxphi/20];
vpospos=[79*maxphi/80: maxphi/320:maxphi];
   if L<=5 %default domain
   contour(X,Y,xysol,30)    
%  contour(X,Y,xysol,[vneg,vpos])
  axis equal
   else
    contour(X,Y,xysol,30)       
   %contour(X,Y,xysol,[vneg,vpos,vpospos])
  %contour(X,Y,xysol,[vneg,vpospos])
   axis equal, axx=ax(1:4); axx(2)=min(25,axx(2));
   axis(axx(1:4)); 
   end
if symm, stepsym, else, stepx, end, axis('off'), axis('off')
title(['Vorticity contours: time = ',num2str(ttk,'%5.2f')],'FontSize',12),  
end
% ------------------ end loop over snapshots
%
fprintf('   All done\n')
return
