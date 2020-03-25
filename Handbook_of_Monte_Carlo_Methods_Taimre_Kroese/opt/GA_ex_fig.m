%GA_ex_fig.m
%Binary Encoded Genetic Algorithm example 
%on SAT problem encoded in the *sparse* matrix A
%Assumes A is loaded in the workspace
N=10^4; % Population Size
K=2; % Size of Tournaments for Selection
M=N; % Size of Reproduction Pool
n=size(A,2); % Dimension of the problem
pm=1/n; % Mutation probability
maxits=10^3; % Maximum number of iterations
X=double(rand(N,n)<=0.5); % Initialize X's Uniform on {0,1}^n
S=sC(A,X); [S,idx]=sort(S,1,'descend'); % Evaluate X & Sort
t=0; % Iteration Counter
Y=zeros(M,n); % Allocate memory for reproduction pool
Smaxhist=zeros(1,maxits); % Allocate history memory
Sminhist=zeros(1,maxits);
Smaxhist(t+1)=S(N);Sminhist(t+1)=S(1);
while (t<maxits)
    % Construct reproduction pool via Tournament Selection
    for k=1:M
    % Generate the K Tournament Participants
        rp=zeros(1,K);
        Ki=K;
        for i=1:n
            if (n-i+1)*rand<=Ki
                rp(Ki)=i;
                Ki=Ki-1;
            end
        end
        Y(k,:)=X(idx(min(rp)),:);
    end
    % Apply one-point crossover 
    for k=1:N
    % Determine the two Parents
        ip=zeros(1,2);
        Ki=2;
        for i=1:n
            if (n-i+1)*rand<=Ki
                ip(Ki)=i;
                Ki=Ki-1;
            end
        end
        i1=ip(1);i2=ip(2); % Parents from Reproduction Pool Y
        cp=floor(rand(1)*(n+1));
        X(k,(1:1:cp))=Y(i1,(1:1:cp));
        X(k,(cp+1:1:n))=Y(i2,(cp+1:1:n));
    end
    % Apply mutations & create new generation
    R=rand(N,n)<=pm; X(R)=1-X(R);
    S=sC(A,X); [S,idx]=sort(S,1,'descend'); % Evaluate X & Sort
    t=t+1;
    Smaxhist(t+1)=S(N);Sminhist(t+1)=S(1);
    [t,S(N),S(1),var(S)]
end
X(idx(1),:)
figure,plot((0:1:maxits),Sminhist,'r-',(0:1:maxits),Smaxhist,'k-')