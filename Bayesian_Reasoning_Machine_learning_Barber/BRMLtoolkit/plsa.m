function [tpxgz,tpygz,tpz,tpxy]=plsa(pxy,Z,opts)
%PLSA Probabilstic Latent Semantic Analysis
% [tpxgz,tpygz,tpz,tpxy]=plsa(pxy,Z,opts)
%
% Inputs:
% pxy : frequency count matrix p(x,y)
% Z : latent dimension 
% opts.tol : convergence tolerance
% opts.plotprogress : if 1 plot the `likelihood'
% opts.maxit : maximum iterations
% opts.randinit : set to 1 for random initialisation -- otherwise it takes
% the values from opts.tpxgz, opts.tpygz, opts.tpz
%
% Outputs:
% tpxgz : approximation of p(x|z)
% tpygz : approximation of p(y|z)
% tpz : approximation of p(z)
% tpxy : approximation of p(x,y)
%
% See also plsaCond.m
[X Y]=size(pxy);

if opts.randinit
	tpz=rand(Z,1); tpz=tpz/sum(tpz);
	tpxgz=rand(X,Z); tpxgz=tpxgz./repmat(sum(tpxgz),X,1);
	tpygz=rand(Y,Z); tpygz=tpygz./repmat(sum(tpygz),Y,1);
else
	tpz=opts.tpgz;
	tpxgz=opts.tpxgz;
	tpygz=opts.tpygz;
end

for emloop=1:opts.maxit
	tpxy=zeros(X,Y);
	for z=1:Z
		tpxy =tpxy+tpxgz(:,z)*tpygz(:,z)'.*tpz(z);
	end
	L(emloop) =sum(sum(pxy.*logeps(tpxy))); % log `likelihood'
	
	% E-step:
	for z=1:Z
		qzgxy(z,:,:)=tpxgz(:,z)*tpygz(:,z)'.*tpz(z)+eps;
	end
	for z=1:Z
		qzgxy(z,:,:)=squeeze(qzgxy(z,:,:))./repmat(sum(sum(qzgxy(z,:,:),3),2),X,Y);
	end
	qzgxy=qzgxy./repmat(sum(qzgxy),Z,1); 

	% M-step:
	for z=1:Z
		tpxgz(:,z)=sum(pxy.*squeeze(qzgxy(z,:,:)),2);
		tpygz(:,z)=sum(pxy.*squeeze(qzgxy(z,:,:)),1);
	end
	tpz=sum(tpxgz)';
	tpxgz=tpxgz./repmat(sum(tpxgz),X,1);
	tpygz=tpygz./repmat(sum(tpygz),Y,1);

	if opts.plotprogress; plot(L); ylabel('log likelihood');drawnow; end
	if emloop>1
		if L(emloop)-L(emloop-1)<opts.tol; break; end
	end
end

