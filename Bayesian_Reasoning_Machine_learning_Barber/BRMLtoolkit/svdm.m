function [U S V]=svdm(X,M,opts)
%SVDM Singular Value Decomposition with missing values
% [U S V]=svdm(X,M,opts)
%
% Singular Value Decompostion for matrix X with missing data (nan)
% M -- number of retained singular values 
% opts.maxiterations, opts.tolerance, opts.displayerror
if  length(find(isnan(X)))==0 % no missing data, just do standard SVD
    [U,S,V]=svds(X,M);
    lambda=diag(S);
else
    if ~isfield(opts,'maxiterations')
        opts.maxiterations=20;
    end
    if ~isfield(opts,'tolerance')
        opts.tolerance=0.001;
    end
    if ~isfield(opts,'displayerror')
        opts.displayerror=0;
    end
    [D N]=size(X);
    G=sparse(D,N); G(isnan(X))=1;  % indices of missing data

    % initialisation for basis:
    m = mynanmean(X,2)';  % mean of missing data:
    XX=X;
    for n=1:N
        zr = find(G(:,n)==1);
        XX(zr,n)=m(zr); % replace missing data by mean
    end
    [B S V]=svds(XX,M);

    old_msqerror=nan;
    for loop=1:opts.maxiterations
        for n=1:N % update W (components)
            comprow=find(G(:,n)==0); % complete rows
            Mn = B(comprow,:)'*B(comprow,:);
            cn=B(comprow,:)'*X(comprow,n);
            W(:,n)=pinv(Mn)*cn;
        end
        for d=1:D % update B (basis)
            goodn=find(G(d,:)==0); % complete columns
            Wg = W(:,goodn);
            Fd = Wg*Wg';
            md=W(:,goodn)*X(d,goodn)';
            B(d,:)=(pinv(Fd)*md)';
        end
        msqerror=mynanmean(mynanmean((X - B*W).^2));
        if opts.displayerror
            fprintf(1,'Iteration %d: mean square reconstruction of non missing elements = %g\n',loop,msqerror);
        end
        if abs(msqerror-old_msqerror)<opts.tolerance
            break;
        else
            old_msqerror = msqerror;
        end
    end
    [U S V]=svds(B*W,M); % return in SVD form
end