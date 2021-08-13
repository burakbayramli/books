function symstep_flowmovie(qmethod,sol,tt,By,Bx,A,xy,x,y,...
						bound,bndxy,bnde,obs,contourn,ftime,fign,avi)
%SYMSTEP_FLOWMOVIE movie of flow around square obstacle
%   symstep_flowmovie(qmethod,U,time,By,Bx,A,xy,x,y,bound,bndxy,bnde,obs,20,.01,998,1);
%   input
%          qmethod    mixed method 
%          U          velocity solution vector
%          time       solution time vector
%          By         velocity  y-derivative matrix    
%          Bx         velocity x-derivative matrix    
%          A          vector diffusion matrix
%          xy         velocity nodal coordinate vector   
%          x          vector of x-axis interpolation points
%          y          vector of y-axis interpolation points
%          bound      boundary vertex vector
%          contourn   number of streamlines
%          ftime      controls speed of animation
%          fign       figure number
%          avi        0/1 switch to generate .avi file
%
% pressure solution is assumed to be essentially zero at outflow 
% so streamfunction satisfies zero Neumann condition there
% calls function xxstreambc.m to set boundary values
%   IFISS function: DJS; 27 July 2015.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('\nGenerating flow field movie ... ')
fprintf('\ndo not adjust the figure ...')
gohome
cd plotfiles
if avi==1, vidObj = VideoWriter('symstepflow.avi');
open(vidObj); end
L=max(x); nstep=length(tt);
nvtx=length(xy); nu=2*nvtx; 
Asv=A(1:nvtx,1:nvtx); fzero=zeros(nvtx,1);
%
%% apply boundary conditions to the diffusion matrix
[Abc,fzero]=streambc(Asv,fzero,xy,bound);
[LA,UA]= lu(Abc); 
%fprintf('\n   step      min_phi    max_phi\n')
for k=4:nstep  %% skip first 3 timesteps
u=sol(:,k); ttk=tt(k);
f=[By,-Bx]*u;
[fsv]=xxstreambc(Asv,f,xy,bound,ttk);
phi=UA\(LA\fsv);
%fprintf('  %4i   %12.5f   %9.3e\n', k, min(phi),max(phi));
%
%% plot velocity
figure(fign)
colormap jet
set(gcf,'Position',[5,5,600,600]);
% interpolate to a cartesian product mesh
[X,Y]=meshgrid(x,y);
xysol = griddata(xy(:,1),xy(:,2),phi,X,Y);
if size(obs,1)~=0
   [II,JJ] = findobsXY(obs,X,Y,bndxy); xysol(II,JJ)=nan;
end
maxphi=max(max(xysol)); minphi=min(min(xysol));
phimin=-1/3; phimax=1/3;
vneg=[minphi:(phimin-minphi)/5:phimin];
vpos=[maxphi:(phimax-maxphi)/5:phimax];
vpospos=[phimin: (phimax-phimin)/contourn:phimax];
contour(X,Y,xysol,[vneg,vpos,vpospos]), axis('equal'),%colorbar
title(['Streamlines : ',num2str(tt(k),'%7.2f'),' seconds']), 
hold on
for i = 1:size(bnde,1)
plot([bndxy(bnde(i,1),1), bndxy(bnde(i,2),1)],[bndxy(bnde(i,1),2),bndxy(bnde(i,2),2)],'-k')
end
hold off
%   bndplot(bndxy,bnde); 
   axis([-0.5,10.5,-1.5,1.5])
   pause(ftime)
%% save frame
if avi==1,
% Write each frame to the file.
currFrame = getframe(gcf,[75 75 470 470]);
writeVideo(vidObj,currFrame); end
end
fprintf('\nThe end.\n')
fprintf('step %g : time is %9.3e\n',k,tt(k))
%%
gohome
cd plotfiles
if avi==1, close(vidObj);
fprintf('\nMovie created: /plotfiles/symstepflow.avi\n'),end
return
