function [xo,fo]=genetic(f,x0,l,u,Np,Nb,Pc,Pm,eta,kmax)
% Genetic Algorithm to minimize f(x) s.t. l<=x<=u
N=length(x0); 
if nargin<10, kmax=100; end %# of iterations(generations)
if nargin<9|eta>1|eta<=0, eta=1; end %learning rate(0<eta<1)
if nargin<8, Pm=0.01; end %probability of mutation
if nargin<7, Pc=0.5; end %probability of crossover
if nargin<6, Nb=8*ones(1,N); end %# of genes(bits) for each variable )
if nargin<5, Np=10; end %population size(number of chromosomes)
%Initialize the population pool
NNb=sum(Nb); 
xo=x0(:)'; l=l(:)'; u=u(:)';
fo=feval(f,xo);
X(1,:)=xo; 
for n=2:Np, X(n,:)=l+rand(size(x0)).*(u-l);  end %Eq.(7.1-26)
P=gen_encode(X,Nb,l,u); %Eq.(7.1-27)
for k=1:kmax
  X=gen_decode(P,Nb,l,u); %Eq.(7.1-28)
  for n=1:Np, fX(n)=feval(f,X(n,:)); end
  [fxb,nb]=min(fX); %Selection of the fittest
  if fxb<fo, fo=fxb; xo=X(nb,:); end
  fX1=max(fxs)-fX; %make the nonnegative fitness vector by Eq.(7.1-29)
  fXm=fX1(nb);
  if fXm<eps, return; end %terminate if all the chromosomes are equal
  %Reproduction of next generation
  for n=1:Np
     X(n,:)=X(n,:)+eta*(fXm-fX1(n))/fXm*(X(nb,:)-X(n,:)); %Eq.(7.1-30)
  end
  P=gen_encode(X,Nb,l,u);
  %Mating/Crossover
  is=shuffle([1:Np]);
  for n=1:2:Np-1   
    if rand<Pc, P(is(n:n+1),:)=crossover(P(is(n:n+1),:),Nb); end
  end
  %Mutation
  P=mutation(P,Nb,Pm);
end
