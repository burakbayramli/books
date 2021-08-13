%DELTA_PLATE solve clamped plate problem with point load
%   IFISS scriptfile: DJS; 9 January 2019.
% Copyright (c) 2018 D.J. Silvester, P. Nadukandi
clear variables
%% define geometry
pde=0; domain=1;
plate_domain
load plate_grid.mat
%
%% set up matrices
qmethod=3;
tic, [A,Q,f] = femq3_matrices(xy,ev); ptoc=toc;
fprintf('fast system setup in %8.3e seconds\n\n',ptoc)
%
%% boundary conditions
   [Agal,fgal] = clampedbc(A,f,xy);
%% impose delta function at midpoint
nvtx=length(xy(:,1)); k1=(nvtx+1)/2; fgal=zeros(size(f)); fgal(k1)=1;
%
%% compute solution
tic
%fprintf('solving linear system ...  ')
x_gal=Agal\fgal;
%fprintf('done\n')
etoc=toc; fprintf('Galerkin system solved in  %8.3e seconds\n\n',etoc) 
%
savesol=default('save results for reference 1/0 (yes/no)? (default no)',0);
if savesol==1,
    xy_ref=xy; Aref=A; x_ref=x_gal;
    save platesolution.mat xy_ref Aref x_ref
    fprintf('solution data saved in platesolution.mat\n')
end
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
