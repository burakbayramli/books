function step_unsteadyflowplot(qmethod,ev,sol,tt,By,Bx,G,xy,xyp,x,y,bound,ftime,fig,symm)
%STEP_UNSTEADYFLOWPLOT evolves flow data on symmetric or standard step domain
%   step_unsteadyflowplot(qmethod,mv,U,time,By,Bx,G,xy,xyp,x,y,bound,ftime,fig,symm);
%   input
%          qmethod    mixed method 
%          mv         mv/ev  Q2/Q1 element mapping matrix
%          U          flow solution vector
%          time       snapshot time vector
%          By         velocity  y-derivative matrix    
%          Bx         velocity x-derivative matrix    
%          G          veclocity mass matrix
%          xy         velocity nodal coordinate vector  
%          xyp        pressure nodal coordinate vector  
%          x          vector of x-axis interpolation points
%          y          vector of y-axis interpolation points
%          bound      boundary vertex vector
%          ftime      controls speed of animation
%          fig        figure number
%          symm       symmetric domain -1/0/1 switch (optional)
%
%   IFISS function: DJS; 20 September 2016.
% Copyright (c) 2014 D.J. Silvester, H.C. Elman, A. Ramage
if nargin < 15, symm = 0; end
fprintf('\nrunning flow animation ... ')
L=max(x); nstep=length(tt);
nvtx=length(xy); nu=2*nvtx; np=length(xyp);
[LG,UG]= lu(G(1:nvtx,1:nvtx)); 
fprintf('\n   step   mean_vorticity\n')
for k=1:nstep
% compute auxilliary quantites
u=sol(:,k);
ux=u(1:nvtx); uy=u(nvtx+1:nu);  utotal=sqrt(ux.*ux+ uy.*uy);
fsv=-[By,-Bx]*u;
omega=UG\(LG\fsv);
%
%%%  
if qmethod > 1,    
   wev = vorticity_q2(xy,ev,omega,0);
else
   wev = vorticity_q1(xy,ev,omega,0);
end
fprintf('  %4i    %11.3e \n', k, sum(wev));
%
% interpolate to a cartesian product mesh
[X,Y]=meshgrid(x,y);
figure(fig)
colormap jet
%
%% plot velocity magnitude
ax = [min(x)-.1 max(x)+.1 min(y)-.1 max(y)+.1];
xysol = griddata(xy(:,1),xy(:,2),utotal,X,Y);
   if symm==1, % symmetric step
   [II,JJ]=find(X<0 & Y<-0.5); xysol(II,JJ)=nan;
   [II,JJ]=find(X<0 & Y>0.5); xysol(II,JJ)=nan;
   elseif symm==0, %regular step
   [II,JJ]=find(X<0 & Y<0); xysol(II,JJ)=nan;
   end
solheight = max(max(xysol))-min(min(xysol));
subplot(211), contour(X,Y,xysol,20)
axis equal, axx=ax(1:4); axx(2)=min(25,axx(2));
axis(axx(1:4));
if symm==1, stepsym, elseif symm==0, stepx, else, chanx, end, axis('off')
title(['velocity magnitude : ', num2str(tt(k)),' seconds'],'FontSize',12)
%
%% plot vorticity
xysol = griddata(xy(:,1),xy(:,2),omega,X,Y);
solheight = max(max(xysol))-min(min(xysol));
ax5 = min(min(xysol))-.1*solheight;
ax6 = max(max(xysol))+.1*solheight;
ax = [min(x)-.1 max(x)+.1 min(y)-.1 max(y)+.1 ax5 ax6];
   if symm==1, % symmetric step
   [II,JJ]=find(X<0 & Y<-0.5); xysol(II,JJ)=nan;
   [II,JJ]=find(X<0 & Y>0.5); xysol(II,JJ)=nan;
   elseif symm==0, %regular step
   [II,JJ]=find(X<0 & Y<0); xysol(II,JJ)=nan;
   end
maxphi=max(max(xysol)); minphi=min(min(xysol));
vneg=[minphi:-minphi/12:0];
vpos=[maxphi/20:maxphi/20:19*maxphi/20];
vpospos=[79*maxphi/80: maxphi/320:maxphi];
subplot(212)
if L<=5 %default domain
  contour(X,Y,xysol,[vneg,vpos])
%  axis('tight')
  %view(-44,18), ax(5)=-4.5; ax(6)=6;
  %axis(ax)
else
   contour(X,Y,xysol,[vneg,vpos,vpospos])
  %contour(X,Y,xysol,[vneg,vpospos])
   axis equal, axx=ax(1:4); axx(2)=min(25,axx(2));
   axis(axx(1:4)); 
end
title(['vorticity : ', num2str(tt(k)),' seconds'],'FontSize',12)
if symm==1, stepsym, elseif symm==0, stepx, else, chanx, end, axis('off')
drawnow, pause(ftime)
end
fprintf('\nAll done\n')
fprintf('step %g : time is %9.3e\n',k,tt(k))
fprintf('minimum w is %g ',min(omega))
fprintf('and maximum w is %g\n',max(omega))
return
