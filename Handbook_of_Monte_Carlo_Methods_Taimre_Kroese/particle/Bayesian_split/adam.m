function [ell,gam,X]=adam(N,Gamma,rho)
%ADAM algorithm
%output: estimated 'ell';
%        a vector of estimated levels 'gam';
%        final population X with approximate distribution f_T
for k=1:N
    [Xp,Sp]=nominal_pdf;  %sample from the nominal pdf f(x)
    X(k,:)=Xp;     Score(k)=Sp;
end
% determine gamma as per step 1 of ADAM
gam(1)=threshold(Score,rho,Gamma);
I=Score>=gam(1);
Nt=sum(I); c(1)=Nt/N; X=X(I,:); Score=Score(I);
S_best=inf;  G=nan(1,15);

for t=2:10^3
    SPLITS=stratified_split(N,Nt); %generate splitting factors
    for i=1:Nt
        Xp=X(i,:);
        for chain_length=1:SPLITS(i)
            % apply markov transition pdf Step 2 of ADAM
            Xp=mcmc(Xp,gam(t-1));
            X(end+1,:)=Xp; Score(end+1)=S(Xp);
        end
        X(1,:)=[]; Score(1)=[];
    end
    % compute new level/threshold
    gam(t)=threshold(Score,rho,Gamma);
    I=Score>=gam(t);Nt=sum(I);
    c(t)=Nt/length(Score); X=X(I,:); Score=Score(I);
    
    [c(t),gam(t)] % monitor output
    
    if gam(t)==Gamma break, end
end
ell=prod(c);