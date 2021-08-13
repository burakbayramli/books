function box_unsteadyflowref(qmethod,ev,sol,tt,A,By,Bx,G,xy,xyp,x,y,bound,hty,snaptime)
%BOX_UNSTEADYFLOWREF plots cavity flow data at snapshot times 
%   box_unsteadyflowref(qmethod,mv2,U,soltime,Av,BBy,BBx,Qv,xyv,xyp,x,y,bnd_d,hty,snaptime);
%   input
%          qmethod    mixed method 
%          mv2        mv/ev  Q2/Q1 element mapping matrix
%          U          flow solution vector
%          soltime    snapshot time vector
%          Av         vector diffusion matrix
%          BBy        velocity  y-derivative matrix    
%          BBx        velocity x-derivative matrix    
%          Qv         velocity mass matrix
%          xyv        velocity nodal coordinate vector  
%          xyp        pressure nodal coordinate vector  
%          x          vector of x-axis interpolation points
%          y          vector of y-axis interpolation points
%          bnd_d      boundary vertex vector
%          hty        horizontal/vertical (1/2) hot wall switch 
%          snaptime   vector of snapshot time step levels
%
% calls function xxstreambc.m to set boundary values
% calls function boxx.m to color code imposed temperature
%   IFISS function: DJS; 18 August 2012.
% Copyright (c) 2011 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('\n   Plotting flow field snapshots ... ')
L=max(x); H=max(y); nstep=length(snaptime);
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
if L >= H, indx=100*nstep +10 +k;
else, indx=100 +10*nstep +k; end
subplot(indx)
xysol = griddata(xy(:,1),xy(:,2),phi,X,Y);
maxphi=max(max(xysol)); minphi=min(min(xysol));
fprintf('  %4i  %7.3f  %11.3e   %12.5f  %9.3e\n', ...
            kk, ttk,  sum(wev), min(phi), max(phi));
if L >= H, 
vneg=[minphi:-minphi/8:0];
vpos=[maxphi/8:maxphi/8:maxphi];
contour(X,Y,xysol,[vneg,vpos])
title(['Stationary streamlines: time = ',num2str(ttk,'%5.2f')],'FontSize',12), 
else 
vneg=[minphi:-minphi/20:0];
vnegneg=[minphi:-minphi/40:minphi/20];;
vpos=[maxphi/12:maxphi/12:maxphi];
vpospos=[0: maxphi/48:maxphi/12];
contour(X,Y,xysol,[vneg,vpos])
title(['time = ',num2str(ttk,'%5.2f')],'FontSize',12),
end
axis('equal'), boxx, axis('off')
%
% plot vorticity
figure(102)
subplot(indx)
xysol = griddata(xy(:,1),xy(:,2),omega,X,Y);
solheight = max(max(xysol))-min(min(xysol));
contour(X,Y,xysol,20)
axis('equal'), boxx, axis('off')
title(['Vorticity: time = ',num2str(ttk,'%5.2f')],'FontSize',12),  
end
% ------------------ end loop over snapshots
%
fprintf('   All done\n')
return
