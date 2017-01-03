function demoPLSAcond
%DEMOPLSACOND demo of conditional PLSA
figure
% Generate a `frequency' matrix from a known basis dimension:
X=50; Y=200; Z=10;
pxgz=rand(X,Z); pxgz=pxgz./repmat(sum(pxgz),X,1);
pzgy=rand(Z,Y); pzgy=pzgy./repmat(sum(pzgy),Z,1);
pxgy=zeros(X,Y);
for z=1:Z
	pxgy =pxgy+pxgz(:,z)*pzgy(z,:);
end
% See if PLSA can recover the basis, given only the frequency matrix:
opts.maxit = 200;
opts.tol=0.000001;
opts.plotprogress=1;
opts.randinit=1;
[tpxgz,tpzgy,tpxgy]=plsaCond(pxgy,Z,opts);

figure
subplot(1,2,1); imagesc(pxgy); title('original p(x|y)')
subplot(1,2,2); imagesc(tpxgy);title('approximate tp(x|y)')