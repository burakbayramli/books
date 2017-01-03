function demoLinearCRF
%DEMOLINEARCRF demo of fitting a linear chaing Conditional Random Field
figure
% make some training data by sampling from a linear Chain CRF
dimx=5;dimy=3;
lambdatrue=10*randn(dimx*dimy+dimy*dimy,1);
N=5;
for n=1:N
    clear phi
    %T(n)=ceil(10*rand)+2; % length of the sequence
    T(n)=20; % length of the sequence
    x{n}=randgen(ones(1,dimx),1,T(n));
    for t=2:T(n)
        phi(t-1)=linearCRFpotential(x{n},t,lambdatrue,dimx,dimy);
    end
    y{n}=potsample(phi,1);  % draw a sample
end

% gradient ascent training
eta=0.1;
lambda=zeros(dimx*dimy+dimy*dimy,1); % initialise the parameters
for n=1:N
    clear phi
    for t=2:T(n)
        phi(t-1)=linearCRFpotential(x{n},t,lambdatrue,dimx,dimy);
    end
    A{n} = FactorGraph(phi); % get the structure of the factor graph for the gradient
end
for loop=1:50
    lambda=lambda + eta*linearCRFgrad(x,y,lambda,A,dimx,dimy); % gradient ascent
    loglik(loop)=linearCRFloglik(x,y,lambda,A,dimx,dimy);
    plot(loglik,'-o'); drawnow
end
% Find the most likely output sequence given each training input sequence
figure
for n=1:N
    clear phi; for t=2:T(n); phi(t-1)=linearCRFpotential(x{n},t,lambda,dimx,dimy); end
    fprintf(1,'training sequence %d:\n',n)
    disp(['input         :',num2str(x{n}(:)')]);
    disp(['true output   :',num2str(y{n}(:)')]);
    ymax{n}=maxprodFG(phi,A{n},[]);
    disp(['CRF MAP output:',num2str(ymax{n})]);
        out(1,:)=x{n};
    out(2,:)=y{n};
    out(3,:)=ymax{n};
    subplot(N,1,n);imagesc(out); 
end
fprintf(1,'\nTest the generalisation:\n');
for n=1:N
    clear phi
    %T(n)=ceil(10*rand)+2; 
    T(n)=20;
    xtest{n}=randgen(ones(1,dimx),1,T(n));
    for t=2:T(n)
        phi(t-1)=linearCRFpotential(xtest{n},t,lambdatrue,dimx,dimy);
    end
    ytest{n}=potsample(phi,1);  % draw a sample
end
% Find the most likely output sequence given each training input sequence
figure
for n=1:N
    clear phi; for t=2:T(n); phi(t-1)=linearCRFpotential(xtest{n},t,lambda,dimx,dimy); end
    fprintf(1,'test sequence %d:\n',n)
    disp(['input         :',num2str(xtest{n}(:)')]);
    disp(['true output   :',num2str(ytest{n}(:)')]);
    ytestmax{n}=maxprodFG(phi,A{n},[]);
    disp(['CRF MAP output:',num2str(ytestmax{n})]);
    out(1,:)=xtest{n};
    out(2,:)=ytest{n};
    out(3,:)=ytestmax{n};
    subplot(N,1,n); imagesc(out); 
end
