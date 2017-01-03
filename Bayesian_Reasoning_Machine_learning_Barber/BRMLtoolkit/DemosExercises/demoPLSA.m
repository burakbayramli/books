function demoPLSA
%DEMOPLSA demo of Probabilistic latent semantic analysis
figure
% Generate a `frequency' matrix from a known basis dimension:
X=100; Y=50; Z=10;
pz=rand(Z,1); pz=pz/sum(pz);
pxgz=rand(X,Z); pxgz=pxgz./repmat(sum(pxgz),X,1);
pygz=rand(Y,Z); pygz=pygz./repmat(sum(pygz),Y,1);
pxy=zeros(X,Y);
for z=1:Z
	pxy =pxy+pxgz(:,z)*pygz(:,z)'.*pz(z);
end
% See if PLSA can recover the basis, given only the frequency matrix:
opts.maxit = 100;
opts.tol=0.000001;
opts.plotprogress=1;
opts.randinit=1;
[tpxgz,tpygz,tpz,tpxy]=plsa(pxy,Z,opts);

figure
subplot(1,2,1); imagesc(pxy); title('original p(x,y)')
subplot(1,2,2); imagesc(tpxy);title('approximate tp(x,y)')