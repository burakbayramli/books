%DE_ex.m
M=50; % Population Size
n=50; % Dimension of the problem
F=0.8; % Scaling factor
CR=0.9; % Crossover factor
maxits=5*10^4; % Maximum number of iterations
Smaxhist=NaN.*ones(1,maxits); Sminhist=NaN.*ones(1,maxits);
% Rosenbrock Function
S=@(X)sum(100.*(X(:,2:n)-X(:,1:(n-1)).^2).^2+(X(:,1:(n-1))-1).^2,2);
a=-50; b=50; X=(b-a).*rand(M,n)+a; % Initialize population
t=1; % Iteration Counter
while (t<maxits)
    SX=S(X); [SX,idx]=sort(SX,1,'ascend'); % Score and Sort
    Smaxhist(t)=SX(M); Sminhist(t)=SX(1); % Update histories
    % Construct the new generation
    for i=1:M
        % Mutation
        r=[1:i-1,i+1:M];
        r=r(randperm(M-1));
        V=X(r(1),:)+F.*(X(r(2),:)-X(r(3),:));
        % Binomial Crossover
        U=X(i,:);
        idxr=1+floor(rand(1).*n);
        for j=1:n
            if (rand(1)<=CR)||(j==idxr)
                U(j)=V(j);
            end
        end
        if S(U)<=S(X(i,:))
            X(i,:)=U;
        end
    end
    t=t+1;
end
SX=S(X); [SX,idx]=sort(SX,1,'ascend'); % Score and Sort
Smaxhist(t)=SX(M); Sminhist(t)=SX(1); % Update histories
% Display worst & best score, and best performing sample
[SX(M),SX(1),X(idx(1),:)]
% Plot the results
figure, plot((1:1:t),Smaxhist,'k-',(1:1:t),Sminhist,'r-')