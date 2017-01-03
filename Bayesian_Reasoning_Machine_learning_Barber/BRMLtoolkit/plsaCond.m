function [tpxgz,tpzgy,tpxgy]=plsaCond(pxgy,Z,opts)
%PLSACOND Conditional PLSA (Probabilstic Latent Semantic Analysis)
% [tpxgz,tpzgy,tpxgy]=plsaCond(pxgy,Z,opts)
%
% Inputs:
% pxy : frequency count matrix p(x|y)
% Z : latent dimension 
% opts.tol : convergence tolerance
% opts.plotprogress : if 1 plot the `likelihood'
% opts.maxit : maximum iterations
% opts.randinit : set to 1 for random initialisation -- otherwise it takes
% the values from opts.tpxgz, opts.tpzgy
%
% Outputs:
% tpxgz : approximation of p(x|z)
% tpzgy : approximation of p(z|y)
% tpxgy : approximation of p(x|y)
%
% See also plsa.m

% pxgy is a frequency count matrix, Z is the `basis' dimension 
[X Y]=size(pxgy);

if opts.randinit
	tpz=rand(Z,1); tpz=tpz/sum(tpz);
	tpxgz=rand(X,Z); tpxgz=tpxgz./repmat(sum(tpxgz),X,1);
	tpzgy=rand(Z,Y); tpzgy=tpzgy./repmat(sum(tpzgy),Z,1);
else
	tpxgz=opts.tpxgz;
	tpzgy=opts.tpzgy;
end

for emloop=1:opts.maxit
	tpxgy=zeros(X,Y);
	for z=1:Z
		tpxgy =tpxgy+tpxgz(:,z)*tpzgy(z,:);
	end
	L(emloop) =sum(sum(pxgy.*log(tpxgy))); % log `likelihood'
	
	% E-step:
	for z=1:Z
		qzgxy(z,:,:)=tpxgz(:,z)*tpzgy(z,:);
	end
	for z=1:Z
		qzgxy(z,:,:)=squeeze(qzgxy(z,:,:))./repmat(sum(sum(qzgxy(z,:,:),3),2),X,Y);
	end
	qzgxy=qzgxy./repmat(sum(qzgxy),Z,1); 

	% M-step:
	for z=1:Z
		tpxgz(:,z)=sum(pxgy.*squeeze(qzgxy(z,:,:)),2);
		tpzgy(z,:)=(sum(pxgy.*squeeze(qzgxy(z,:,:)),1))';
	end
	tpxgz=tpxgz./repmat(sum(tpxgz),X,1);
	tpzgy=tpzgy./repmat(sum(tpzgy),Z,1);

	if opts.plotprogress; plot(L); ylabel('log likelihood');drawnow; end
	if emloop>1
		if L(emloop)-L(emloop-1)<opts.tol; break; end
	end
end