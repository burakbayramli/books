function[z zbest]=cliquedecomp(A,C,varargin)
%CLIQUEDECOMP Clique matrix decomposition
% A: adjacency matrix
% C: maximum number of clusters required
% opts.zloops: innerloop for a fixed number of cliques
% opts.aloops: outerloop to find optimal number of clusters
% opts.beta: inverse temperature
% opts.burnin: set to 1 to do some initial burn in
% opts.burninbeta: burn in beta value
% opts.a_beta: Beta distribution a parameter
% opts.b_beta: Beta distribution b parameter
% see demoCliqueDecomp.m and cliquedecomp.c
V=size(A,1); opts=[];
if nargin==3; opts=varargin{1}; end
opts=setfields(opts,'zloops',ceil(V/10),'aloops',5,'beta',10,'a_beta',1,'b_beta',10,'plotprogress',1,'burnin',0);% default options
beta=opts.beta; a_beta=opts.a_beta; b_beta=opts.b_beta;
A=A-diag(diag(A)); A=A+eye(size(A,1)); % include self connections
%   Mean Field approximation of p(z_i=1|A)
zm=real(rand(V,C)>0.0); % random initialisation
ma=ones(1,C); zma=zm.*repmat(ma,V,1); errors=-1;
olderror=10e100; zold=zm; zbest=zm; besterror=olderror;
for loopa=1:opts.aloops
    if opts.burnin & loopa==1
        thisbeta=opts.burninbeta;
    else
        thisbeta=beta;
    end
    
    for loopz=1:opts.zloops
           
        Mold = zma*zma';
        for c=randperm(C)
            
            zc=zma(:,c);
            for k=randperm(V)
                Mk = Mold(:,k)-zc(:)*zc(k)+zma(:,c)*zma(k,c);
                logfn1=0;logfn0=0;
                for j=1:V
                    mnfield0= Mk(j)-zma(j,c)*zma(k,c)-0.5;  mnfield1= zma(j,c) +  mnfield0;
                    mnfield0=thisbeta*mnfield0;  mnfield1=thisbeta*mnfield1;
                    if A(j,k)
                        logfn1=logfn1 + logsigma(mnfield1); logfn0=logfn0 + logsigma(mnfield0);
                    else
                        logfn1=logfn1 + logsigma(-mnfield1); logfn0=logfn0 + logsigma(-mnfield0);
                    end % end if
                end % end for j
                zm(k,c) = 1/(1+exp(2*cap(logfn0-logfn1,100)));
                zma(k,c)=zm(k,c)*ma(c);
            end % end for k
            Mold = Mold-zc*zc'+zma(:,c)*zma(:,c)';
        end % end for c
        z=zma>0.49; % threshold
        dz=double(z); AA= (dz*dz' > 0); AA = AA-diag(diag(AA)); AA = AA+diag(ones(1,V));
        newerror= 0.5*sum(sum(AA~=A));
       
        if newerror<besterror
            zbest=z; besterror=newerror;
        else
            z=zbest; newerror=besterror;
        end
        
        errors(loopz) = newerror;
        fprintf(1,'number of cliques=%d (after %d updates) | vertex loop %d| errors %d| best error %d| beta %f\n',...
            [C (loopa-1) loopz  newerror besterror thisbeta]);
        if opts.plotprogress
            subplot(1,4,1);imagesc(A);colormap('bone');title('original');subplot(1,4,2);imagesc(AA);
            colormap('bone');title('approx');subplot(1,4,3);
            imagesc(z);colormap('bone');title('Z');subplot(1,4,4);plot(errors);drawnow;
        end
    end % end zloop
    %sum(z)
    %pause
    
    % update the number of cliques:
    if opts.aloops>1
        zmatmp=zma;
        for c=1:C
            zmat=zmatmp; zmat(:,c)=zm(:,c);
            loglik1=cliqueloglikfn(zmat,A,thisbeta);
            zmat(:,c)=zeros(V,1);
            loglik0=cliqueloglikfn(zmat,A,thisbeta);
            atmp=ma; atmp(c)=1; T1=sum(atmp); T0=C-T1;
            logprior1 = betalog(a_beta+T1,b_beta+T0);
            atmp(c)=0; T1=sum(atmp); T0=C-T1;
            logprior0 = betalog(a_beta+T1,b_beta+T0);
            logpost1=logprior1+loglik1; logpost0=logprior0+loglik0;
            ma(c)=1/(1+exp(logpost0-logpost1));
        end % end aloop
        zma=zma(:,ma>0.5);  C=size(zma,2); % update the number of clusters
        ma=ma(:,ma>0.5);
    end
    keyboard
end % end aloop

function loglik=cliqueloglikfn(z,A,beta)
V=size(A,1); loglik=0;
for i=1:V
    for j=1:V
        if A(i,j) % if connection between i and j
            loglik=loglik+logsigma(beta*(sum(z(i,:).*z(j,:))-0.5));
        else
            loglik=loglik+logsigma(-beta*(sum(z(i,:).*z(j,:))-0.5));
        end
    end
end