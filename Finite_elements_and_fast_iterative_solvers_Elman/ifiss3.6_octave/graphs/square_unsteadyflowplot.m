function square_unsteadyflowplot(qmethod,ev,sol,tt,By,Bx,G,xy,xyp,x,y,bound,ftime,fig)
%SQUARE_UNSTEADYFLOWPLOT plots flow data on square domain
%   square_unsteadyflowplot(qmethod,mv,sol,By,Bx,A,xy,xyp,x,y,bound,ftime,fig);
%   input
%          qmethod    mixed method 
%          ev        mv/ev  Q2/Q1 element mapping matrix
%          sol        flow solution vector
%          tt         solution time vector  
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
%
%   IFISS function: DJS; 27 July 2015
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('\nrunning flow field animation ... ')
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
xysol = griddata(xy(:,1),xy(:,2),utotal,X,Y);
solheight = max(max(xysol))-min(min(xysol));
subplot(121), contour(X,Y,xysol,25), axis('square')
squarex, axis('off')
title(['Velocity magnitude : ', num2str(tt(k)),' seconds'])
%
%% plot vorticity
xysol = griddata(xy(:,1),xy(:,2),omega,X,Y);
maxphi=max(max(xysol)); minphi=min(min(xysol));
vneg=[minphi:-minphi/6:0];
vpos=[maxphi/20:maxphi/20:19*maxphi/20];
subplot(122)
%contour(X,Y,xysol,[vneg,vpos,vpospos])
contour(X,Y,xysol,20)
axis('square')
title(['Vorticity : ', num2str(tt(k)),' seconds'])
squarex; axis('off')
drawnow, pause(ftime)
end
fprintf('\nAll done\n')
fprintf('step %g : time is %9.3e\n',k,tt(k))
fprintf('minimum w is %g ',min(omega))
fprintf('and maximum w is %g\n',max(omega))
return
