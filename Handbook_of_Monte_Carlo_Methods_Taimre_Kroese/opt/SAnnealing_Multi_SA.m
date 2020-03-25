function SAnnealing_Multi_SA
% Illustrate performance of multiple MCMC samplers for SA

% Initialization
[T0,S0,X0,minfactor]=initialize();

maxits=10^6;
trials=10^1;

histS1=zeros(trials,maxits+2);
histT1=zeros(1,maxits+2);
histS2=zeros(trials,maxits+2);
histT2=zeros(1,maxits+2);
histN1=zeros(trials,maxits+2);
histN2=zeros(trials,maxits+2);

for tr=1:trials
    for type=1:2
    
        notstop=true;
    
        T=T0;S=S0;X=X0;nstep=0;
    
        k=0; % Iteration Counter
    
        while (notstop)
            Xold=X; Sold=S; Told=T;
        
            eval(['histS' num2str(type) '(' num2str(tr) ',k+1)=S;'])
            eval(['histT' num2str(type) '(k+1)=T;'])        
            eval(['histN' num2str(type) '(' num2str(tr) ',k+1)=nstep;'])        
            
            if mod(k,10^3)==0
                fprintf('%d of %d...\n',k,maxits);
            end
    
            % Generate New State
            [X,nstep]=generate(k,T,S,X,minfactor,type);
            S=Score(X);
        
            % Select New Temperature
            T=temperature(k,T,S,X,Told,Sold,Xold);
            k=k+1; % Increment Iteration
        
            % Check Stopping Criteria
            notstop=notstopping(k,T,S,X,Told,Sold,Xold,maxits);
        end
    
        eval(['histS' num2str(type) '(' num2str(tr) ',k+1)=S;'])
        eval(['histT' num2str(type) '(k+1)=T;'])                
        eval(['histN' num2str(type) '(' num2str(tr) ',k+1)=nstep;'])        
        [X,S,T]
    end
end

figure(1),
plot((0:1:maxits+1),min(histS1,[],1),'k-',(0:1:maxits+1),max(histS1,[],1),'k-'),hold on
plot((0:1:maxits+1),min(histS2,[],1),'b-',(0:1:maxits+1),max(histS2,[],1),'b-'),
plot((0:1:maxits+1),mean(histS1,1),'k:',(0:1:maxits+1),mean(histS2,1),'b:'),
hold off

figure(2),
plot((0:1:maxits+1),min(histT1,[],1),'k-')

figure(3),
plot((0:1:maxits+1),min(histN1,[],1),'k-',(0:1:maxits+1),max(histN1,[],1),'k-'),hold on
plot((0:1:maxits+1),min(histN2,[],1),'b-',(0:1:maxits+1),max(histN2,[],1),'b-'),
plot((0:1:maxits+1),mean(histN1,1),'k:',(0:1:maxits+1),mean(histN2,1),'b:'),
hold off


function [out,nstep]=generate(k,T,S,X,minfactor,type)
% Generate X_{k+1} from f_{T_k}(x) \propto \exp(minfactor*S(x)/T_k)

d=length(X);
nstep=0;

switch(type) 

    case 1
        % AR to generate exactly!
        
        xstar=10.*ones(1,d);
        eta=0.8;
        mu=0.1;
        
        C=(T/mu)^2;
        
        out=zeros(1,d);
        for i=1:d
    
            reject=true;
            while (reject)
                Y=-T*log(rand)/mu;
                U=rand;
                if (C*(mu/T)*exp(-(mu/T)*Y)*U<=exp(-(mu/T)*Y-(6*(sin(2*eta*Y)^2)+8*(sin(eta*Y)^2))/T))
                    reject=false;
                end
            end
            out(i)=xstar(i)+(2*(rand<=0.5)-1).*sqrt(Y);
        end
    
    case 2
        
        % Uses Metropolis-Hastings, with a multivariate Gaussian proposal,
        % with mean X_k and Covariance matrix equal to sigma^2*I

        sigma=ones(1,d).*0.75;

	N=1; %Number of steps to perform of MH

	it=0;
        while (it<N)
        
            Y=X+sigma.*randn(1,d);
            Sy=Score(Y);
            
            alpha=min(1,exp(minfactor*(Sy-S)/T));
            if rand<=alpha
               X=Y; 
               S=Sy;
               nstep=nstep+1;
            end
            
            it=it+1;
        end
        
        out=X;
        
end


function out=notstopping(k,T,S,X,Told,Sold,Xold,maxits)
% Returns TRUE if not stopping and FALSE if 
% stopping criteria are met

if (k>maxits)
    out=false;
else
    out=true;
end

function out=temperature(k,T,S,X,Told,Sold,Xold)
% Determine new temperature

% Geometric Progression
factor=0.99999;
out=factor.*T;




function [T,S,X,minfactor]=initialize()
% Generate initial state X and temperature T

d=10;
a=-50;b=50;

% Is this a minimization problem?
% If so, set minfactor = -1
% Otherwise, set it to +1
minfactor=-1; 

T=10;
X=(b-a).*rand(1,d)+a;
S=Score(X);

function out=Score(X)
% Evaluate Objective Function at X

d=length(X);

% Trigonometric Function
xstar=10.*ones(1,d);
eta=0.8;
mu=0.1;

sq=(X-xstar).^2;
out=1+sum(mu.*sq + 6.*(sin(2.*eta.*sq)).^2 + 8.*(sin(eta.*sq)).^2);

