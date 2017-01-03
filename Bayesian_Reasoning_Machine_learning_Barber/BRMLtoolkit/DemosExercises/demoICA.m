function demoICA
% generate some data:
r=-2:0.01:2; p = normp(exp(-5*sqrt(abs(r)))); % prior distribution for the data 
N=2000; H=2;
X0 = r(randgen(p,H,N)); X0 = X0./max(X0(:)); 
A0=[-4 0; 2 -2]; % true mixing matrix 
Y=A0*X0; % observations
scatter(Y(1,:),Y(2,:),2,'k')
ls = 10*sqrt(max(eig(cov(X0')))); % find a length scale for the plots
sA0 = ls*A0./repmat(sqrt(sum(A0.^2)),2,1);
mn=mean(Y,2);
h=line([mn(1)-sA0(1,1) (mn(1)+sA0(1,1))],[mn(2)-sA0(2,1) (mn(2)+sA0(2,1))]); set(h,'color','g','linewidth',1)
h=line([mn(1)-sA0(1,2) (mn(1)+sA0(1,2))],[mn(2)-sA0(2,2) (mn(2)+sA0(2,2))]); set(h,'color','g','linewidth',1)

figure; opts.maxits=500; opts.tol=10e-6; opts.whiten=0; opts.plotprogress=1; [A,B,X]=ica(Y,opts);
figure(1); [dum E]=pca(Y); sA = 0.5*ls*A./repmat(sqrt(sum(A.^2)),2,1); % PCA

h=line([mn(1)-sA(1,1) (mn(1)+sA(1,1))],[mn(2)-sA(2,1) (mn(2)+sA(2,1))]); set(h,'color','r','linewidth',2)
h=line([mn(1)-sA(1,2) (mn(1)+sA(1,2))],[mn(2)-sA(2,2) (mn(2)+sA(2,2))]); set(h,'color','r','linewidth',2)
h=line([mn(1)-E(1,1) (mn(1)+E(1,1))],[mn(2)-E(2,1) (mn(2)+E(2,1))]); set(h,'color','b','linewidth',2,'linestyle','--')
h=line([mn(1)-E(1,2) (mn(1)+E(1,2))],[mn(2)-E(2,2) (mn(2)+E(2,2))]); set(h,'color','b','linewidth',2,'linestyle','--')
set(gca,'box','on'); axis equal
fprintf(1,' Green are the ICA components\n Red the true independent components used to generate the data\n Blue the PCA components\n')