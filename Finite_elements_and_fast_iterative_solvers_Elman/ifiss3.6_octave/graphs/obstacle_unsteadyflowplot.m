function obstacle_unsteadyflowplot(qmethod,ev,sol,tt,By,Bx,G,xy,xyp,x,y,...
                                   bound,bndxy,bnde,obs,ftime,fig)
%OBSTACLE_UNSTEADYFLOWPLOT plots flow data in obstructed channel 
%   obstacle_unsteadyflowplot(qmethod,mv,xns,time,By,Bx,G,xy,xyp,x,y,...
%                                   bound,bndxy,bnde,obs,ftime,fig);
%   input
%          qmethod    mixed method 
%          ev         mv/ev  Q2/Q1 element mapping matrix
%          sol        flow solution vector
%          tt         snapshot time vector
%          By         velocity  y-derivative matrix    
%          Bx         velocity x-derivative matrix    
%          G          velocity mass matrix
%          xy         velocity nodal coordinate vector  
%          xyp        pressure nodal coordinate vector  
%          x          vector of x-axis interpolation points
%          y          vector of y-axis interpolation points
%          bound      boundary vertex vector
%          ftime      controls speed of animation
%          fig        figure number
%
%   IFISS function: DJS; 27 July 2015
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('\nrunning flow field animation ... ')
contourn = default('number of contour lines (default 50)',50);
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
if size(obs,1)~=0
   [II,JJ] = findobsXY(obs,X,Y,bndxy); xysol(II,JJ)=nan;
end
solheight = max(max(xysol))-min(min(xysol));
subplot(212),contour(X,Y,xysol,contourn),axis('tight'),%colorbar
hold on
for i = 1:size(bnde,1)
   plot([bndxy(bnde(i,1),1), bndxy(bnde(i,2),1)],[bndxy(bnde(i,1),2),bndxy(bnde(i,2),2)],'-k')
end
hold off
%bndplot(bndxy,bnde); %ax(2)=min(25,ax(2));
axis(ax), axis('off')
title(['Velocity magnitude : ', num2str(tt(k)),' seconds'])
%
%% plot vorticity
xysol = griddata(xy(:,1),xy(:,2),omega,X,Y);
solheight = max(max(xysol))-min(min(xysol));
ax5 = min(min(xysol))-.1*solheight;
ax6 = max(max(xysol))+.1*solheight;
ax = [min(x)-.1 max(x)+.1 min(y)-.1 max(y)+.1 ax5 ax6];
[II,JJ]=find(X<0 & Y<0); xysol(II,JJ)=nan;
maxphi=max(max(xysol)); minphi=min(min(xysol));
vneg=[minphi:-minphi/12:0];
vpos=[maxphi/20:maxphi/20:19*maxphi/20];
vpospos=[79*maxphi/80: maxphi/320:maxphi];
subplot(211)
if size(obs,1)~=0
   [II,JJ] = findobsXY(obs,X,Y,bndxy); xysol(II,JJ)=nan;
end
subplot(211),contour(X,Y,xysol,contourn),axis('tight'),%colorbar
hold on
for i = 1:size(bnde,1)
   plot([bndxy(bnde(i,1),1), bndxy(bnde(i,2),1)],[bndxy(bnde(i,1),2),bndxy(bnde(i,2),2)],'-k')
end
hold off
%bndplot(bndxy,bnde); %ax(2)=min(25,ax(2));
axis(ax(1:4)), axis('off')
title(['Vorticity : ', num2str(tt(k)),' seconds'])
drawnow, pause(ftime)
end
fprintf('\nAll done\n')
fprintf('step %g : time is %9.3e\n',k,tt(k))
fprintf('minimum w is %g ',min(omega))
fprintf('and maximum w is %g\n',max(omega))
return
