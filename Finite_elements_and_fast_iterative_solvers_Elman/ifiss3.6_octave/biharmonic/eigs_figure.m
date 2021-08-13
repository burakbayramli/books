%EIGS_FIGURE Plots eigenmodes of clamped plate problem in square domain
%   IFISS scriptfile: DJS; 11 September 2018.
% Copyright (c) 2018 D.J. Silvester, P. Nadukandi


nn=sqrt(nvtx);
% plot first three eigenvectors
figure(33)
v1=V(:,1);
u1=reshape(v1(1:nvtx),nn,nn);
subplot(231)
%colormap(hsv)
contour(xy(1:nn,1),xy(nn:nn:nvtx,2),u1), axis square,
h=title(['$\lambda_1$ is ',num2str(ee(1))]);
set(h,'Interpreter','latex')
%
v2=V(:,2);
u2=reshape(v2(1:nvtx),nn,nn);
subplot(232)
%colormap(hsv)
contour(xy(1:nn,1),xy(nn:nn:nvtx,2),u2), axis square,
h=title(['$\lambda_2$ is ',num2str(ee(2))]);
set(h,'Interpreter','latex')
%
v3=V(:,3);
u3=reshape(v3(1:nvtx),nn,nn);
subplot(233)
%colormap(jet)
contour(xy(1:nn,1),xy(nn:nn:nvtx,2),u3), axis square,
h=title(['$\lambda_3$ is ',num2str(ee(3))]);
set(h,'Interpreter','latex')
%
% plot next three eigenvectors
%figure(12)
v1=V(:,4);
%colormap(jet)
u1=reshape(v1(1:nvtx),nn,nn);
subplot(234)
contour(xy(1:nn,1),xy(nn:nn:nvtx,2),u1), axis square,
h=title(['$\lambda_4$ is ',num2str(ee(4))]);
set(h,'Interpreter','latex')
%
v2=V(:,5);
u2=reshape(v2(1:nvtx),nn,nn);
subplot(235)
%colormap(jet)
contour(xy(1:nn,1),xy(nn:nn:nvtx,2),u2), axis square,
h=title(['$\lambda_5$ is ',num2str(ee(5))]);
set(h,'Interpreter','latex')
%
v3=V(:,6);
u3=reshape(v3(1:nvtx),nn,nn);
subplot(236)
%colormap(jet)
contour(xy(1:nn,1),xy(nn:nn:nvtx,2),u3), axis square,
h=title(['$\lambda_6$ is ',num2str(ee(6))]);
set(h,'Interpreter','latex')
