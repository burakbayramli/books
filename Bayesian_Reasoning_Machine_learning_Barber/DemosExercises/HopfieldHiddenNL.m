function [A B C D]=HopfieldHiddenNL(v,H,opts)
%HopfieldHiddenNL Hopfield Net with deterministic non-linear latent variables
% [A B C D]=HopfieldHiddenNL(v,H,opts)
[V T]=size(v);
A = 0.001*(randn(H,H)); B = 0.001*randn(H,V);
C = 0.001*randn(V,H); D = 0.001*randn(V,V);
h(:,1)=zeros(H,1); eta=opts.eta;
dhA = zeros(H,H,H); dhB=zeros(H,H,V);
for loop=1:opts.its
    dhA_old = zeros(H,H,H); gA = zeros(H,H);
    dhB_old = zeros(H,H,V); gB = zeros(H,V);
    gC = zeros(V,H); gD = zeros(V,V);
    
    for t = 1:T-1
        h(:,t+1)= 2*sigma(A*h(:,t)+B*v(:,t))-1;
        at=D*v(:,t) + C*h(:,t); nu = (1-sigma((2*v(:,t+1)-1).*at)).*(2*v(:,t+1)-1);
        tmp= sigma(A*h(:,t)+B*v(:,t)); s = 2*tmp.*(1-tmp);
        
        % A gradient:
        for i=1:H
            for alpha=1:H
                for beta=1:H
                    tmp = sum(A(i,:)'.*dhA_old(:,alpha,beta));
                    if i==alpha; tmp=tmp+h(beta,t); end
                    dhA(i,alpha,beta) = s(i)*tmp;
                end
            end
        end
        dphiA = zeros(V,H,H);
        for i=1:V
            for alpha=1:H
                for beta=1:H
                    dphiA(i,alpha,beta)=sum(C(i,:)'.*dhA_old(:,alpha,beta));
                end
            end
        end
        tmp=zeros(H,H);
        for alpha=1:H
            for beta=1:H
                tmp(alpha,beta)=sum(nu.*dphiA(:,alpha,beta));
            end
        end
        gA=gA+tmp;
        
        % B gradient:
        for i=1:H
            for alpha=1:H
                for beta=1:V
                    tmp=sum(A(i,:)'.*dhB_old(:,alpha,beta));
                    if i==alpha; tmp=tmp+v(beta,t); end
                    dhB(i,alpha,beta) = s(i)*tmp;
                end
            end
        end
        dphiB = zeros(V,H,V);
        for i=1:V
            for alpha=1:H
                for beta=1:V
                    dphiB(i,alpha,beta)=sum(C(i,:)'.*dhB_old(:,alpha,beta));
                end
            end
        end
        tmp=zeros(H,V);
        for alpha=1:H
            for beta=1:V
                tmp(alpha,beta)=sum(nu.*dphiB(:,alpha,beta));
            end
        end
        gB = gB + tmp;
        gC = gC + nu*h(:,t)'; % C gradient
        gD = gD + nu*v(:,t)'; % D gradient
        
        dhA_old=dhA; dhB_old=dhB;
    end
    L(loop)=HopfieldHiddenLikNL(v,A,B,C,D,h(:,1));
    A = A + eta*gA;  B = B + eta*gB; C = C + eta*gC; D = D + eta*gD; % batch update
    if opts.plotprogress; plot(L); title('sequence log likelihood'); drawnow; end
end