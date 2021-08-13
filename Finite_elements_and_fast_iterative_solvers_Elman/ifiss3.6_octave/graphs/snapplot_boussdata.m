%SNAPPLOT_BOUSSDATA plots snapshot Boussinesq flow solution
%   IFISS scriptfile: DJS;  3 May 2012.
% Copyright (c) 2012 D.J. Silvester, M.L. Mihajlovic

% insist on enclosed flow domain
if domain~=7,error('Plotting is not possible on a non-rectangular domain.'), end;
fprintf('Plotting Boussinesq flow in a rectangular domain...\n')
fprintf('\nSolution data available for %g seconds',soltime(end)) 
fprintf('\n              start time is %g seconds\n',soltime(1)) 
snaptime = default('Approximate time for the SNAPSHOT? (default is the end)',soltime(end));
timek=find(snaptime >= soltime,1,'last');
fprintf('Time step number %g\n',timek) 
figno = default('Figure number for streamline SNAPSHOT? (default 11)',11);

%%% unpack grid and matrix data
tout=1;unpack_boussdata
boundv=bnd_d; boundt=bnd_dn2; 
%%
n=soltimek; t=soltime(n); 
dt=solDT(n); dt0=solDT(n-1);
 u=U(:,n); ub=U(:,n-1); 
tt=T(:,n); 
%
%% load assembled matrices
gohome; 
cd datafiles; 
load rect_bouss_nobc.mat; load rect_grid1h.mat 
%

%% unpack grid data
xyv=grid(1).xyv;  nvtx=length(xyv); 
xyp=grid(1).xyp;  np=length(xyp);
xyt=grid(1).xyt;  nt=length(xyt);
%
%% streamfunction
Asv=spmat.Av(1:nvtx,1:nvtx);
f=[spmat.BBy,-spmat.BBx]*u; 
[Asv,fsv]=zerobc(Asv,f,xyv,boundv);
phi=Asv\fsv;
figure(figno)
[X,Y]=meshgrid(grid.x,grid.y);
xysol=griddata(xyv(:,1),xyv(:,2),phi,X,Y);
%list=[-9.507,[-8.646:8.646/9:0]];
%contour(X,Y,xysol,list),axis('image');
contour(X,Y,xysol,12),axis('image');
title(['Velocity streamlines :  time= ',num2str(t,'%7.2f')],'FontSize',12);
boxz, axis('off')
%% isotherms
figure(figno-10)
xysolt = griddata(xyt(:,1),xyt(:,2),tt,X,Y);
contour(X,Y,xysolt,25),axis('image');
if exist('Ra','var')==1,
title(['Isotherms :  Ra=',num2str(Ra),'; ...
	   Pr=',num2str(Pr),';  time=',num2str(t,'%7.2f')],'FontSize',12);
else, title(['Isotherms :  time=',num2str(t,'%7.2f')],'FontSize',12); end
boxx, axis('off')
