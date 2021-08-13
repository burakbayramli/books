function channel_unsteadyflowref(qmethod,sol,tt,By,Bx,A,xy,x,y,...
						bound,bndxy,bnde,snaptime)
%CHANNEL_UNSTEADYFLOWREF plots channel flow data at snapshot times
%   channel_unsteadyflowref(qmethod,U,time,By,Bx,A,xy,x,y,bound,bndxy,bnde,snaptime);
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
%          snaptime   vector of snapshot time step levels
%
% pressure solution is assumed to be essentially zero at outflow 
% so streamfunction satisfies zero Neumann condition there
% calls function xxstreambc.m to set boundary values
%   IFISS function: DJS; 20 September 2016.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('\n   Plotting flow field snapshots ... ')
L=max(x); nstep=length(snaptime);
if nstep>9, error('Too many snapshots!'), end
nvtx=length(xy); nu=2*nvtx; 
Asv=A(1:nvtx,1:nvtx); fzero=zeros(nvtx,1);
%
%% apply boundary conditions to the diffusion matrix
[Abc,fzero]=streambc(Asv,fzero,xy,bound);
[LA,UA]= lu(Abc); 
fprintf('\n   step   time       min_phi    max_phi\n')
%
% ------------------ loop over snapshots
for k=1:nstep
kk=snaptime(k); ttk=tt(kk); u=sol(:,kk);
f=[By,-Bx]*u;
[fsv]=xxstreambc(Asv,f,xy,bound,ttk);
phi=UA\(LA\fsv);
fprintf('  %4i   %7.3f  %10.5f   %9.3e\n', kk, ttk, min(phi),max(phi));
%
% interpolate to a cartesian product mesh
[X,Y]=meshgrid(x,y);
%
% plot stream function
figure(101)
colormap jet
indx=100*nstep +10 +k;
subplot(indx)
xysol = griddata(xy(:,1),xy(:,2),phi,X,Y);
maxphi=max(max(xysol)); minphi=min(min(xysol));
vneg=[minphi:-minphi/12:0];
vpos=[maxphi/12:maxphi/12:11*maxphi/12];
contour(X,Y,xysol,[vneg,vpos]),axis('equal'), axis('off'); %colorbar
title(['stationary streamlines: time = ',num2str(ttk,'%6.3f')],'FontSize',10),
hold on
for i = 1:size(bnde,1)
plot([bndxy(bnde(i,1),1), bndxy(bnde(i,2),1)],[bndxy(bnde(i,1),2),bndxy(bnde(i,2),2)],'-k')
end
hold off
end
% ------------------ end loop over snapshots
fprintf('   All done\n')
return
