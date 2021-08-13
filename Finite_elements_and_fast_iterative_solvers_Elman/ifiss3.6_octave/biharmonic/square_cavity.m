%SQUARE_CAVITY solve Stokes driven cavity flow problem
%   IFISS scriptfile: DJS; 22 July 2021.
% Copyright (c) 2018 D.J. Silvester, P. Nadukandi
clear variables
%% define geometry
pde=0; domain=1;
plate_domain
load plate_grid.mat
%
%% set up matrices
qmethod=3;
[A,Q,f] = femq3_matrices(xy,ev);
% zero load function
f=0*f;
%% boundary conditions
[Agal,fgal] = cavitybc(A,f,xy);
%
%% compute solution
x_gal=Agal\fgal;

%%  solution data
nvtx=length(xy(:,1)); k1=(nvtx+1)/2; k2=3*nvtx+k1;  k3=3*nvtx+1;
fprintf('\ncomputed solution values:')
fprintf('\nu = %9.7f, u_xy =   %8.4e, u_xy* = %8.4e \n\n',...
        x_gal(k1),x_gal(k2),x_gal(k3))
%
% plot solution
nn=sqrt(nvtx);
figure(41)
colormap('white')
u=reshape(x_gal(1:nvtx),nn,nn); uxy=reshape(x_gal(3*nvtx+1:4*nvtx),nn,nn);
subplot(121)
surf(xy(1:nn,1),xy(nn:nn:nvtx,2),u), axis square, title('displacement')
subplot(122)
surf(xy(1:nn,1),xy(nn:nn:nvtx,2),uxy), axis square, title('twisting moment')
% postprocess
[hx,hy,eex] = edgegen(xy,ev);
