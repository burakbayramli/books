function [A]=spc(D,X,erg); 
%sparse coding (OMP)

[n,P]=size(X); [n,K]=size(D);
e2 = erg^2*n;
mnc = n/2; %maximum number of coefficients
A = sparse(K,P); %sparse matrix
for mk=1:P,
    aux=[]; inx=[];
    x=X(:,mk); resd=x;
	rno2 = sum(resd.^2); %residual norm 2
	mj = 0;
    while rno2>e2 & mj < mnc,
		mj = mj+1;
        proj=D'*resd;
        ps=find(abs(proj)==max(abs(proj)));
        ps=ps(1); inx(mj)=ps;
        aux=pinv(D(:,inx(1:mj)))*x;
        resd=x-D(:,inx(1:mj))*aux;
		rno2 = sum(resd.^2);
   end;
   if (length(inx)>0), A(inx,mk)=aux; end;
end;
return;
