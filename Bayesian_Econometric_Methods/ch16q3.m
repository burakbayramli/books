%Program for Exercise 3 in Chapter 16
%Bayesian model averaging program
%Use cross country growth example from Fernandez, Ley and Steel (JAE, 2001)

load growth.dat;
%The data set is arranged with data for each country taking up 6 lines
%The following makes it into an N by K matrix
n=72;
%There probably is a better way of doing this...
rawdat=zeros(n,42);
j=1;
for i=1:n
    rawdat(i,:)= [growth(j,:) growth(j+1,:) growth(j+2,:) ...
            growth(j+3,:) growth(j+4,:) growth(j+5,:)];
    j=j+6;
end

y=rawdat(:,1);
xraw=rawdat(:,2:42);
bigk=size(xraw,2);

%subtract mean from all regressors as in FLS
mxraw=mean(xraw);
for i=1:bigk
    xraw(:,i)=xraw(:,i) - mxraw(1,i);
end


%I am creating a vector of length bigk which is 1 if explanatory
%variable is included, else equals zero
%molddraw is initialized to contain all explanatory variables
%with t-stats greater than .5

molddraw=zeros(bigk,1);
xold=[ones(n,1) xraw];
xtxinv=inv(xold'*xold);
bhat=xtxinv*xold'*y;
e=y-xold*bhat;
sse=e'*e;
s2=sse/(n-bigk-1);
bcov=s2*xtxinv;
bt=zeros(bigk+1,1);
for i=2:bigk+1
    bt(i,1)=bhat(i,1)/sqrt(bcov(i,i));
    if abs(bt(i,1))>.5
        molddraw(i-1,1)=1;
    end
end

%Make up the matrix of explanatory variables for model
xold=ones(n,1);
kold=sum(molddraw)+1;
for i=1:bigk
    if molddraw(i,1)>0
        xold=[xold xraw(:,i)];
    end
end
%specify g0 for the g-prior
if n<=(bigk^2)
    g0=1/(bigk^2);
else
    g0=1/n;
end

yty = (y-mean(y))'*(y-mean(y));
xtxinv=inv(xold'*xold);
ymy = y'*y - y'*xold*xtxinv*xold'*y;
g1=g0/(g0+1);
g2=1/(g0+1);
lprobold = .5*kold*log(g1) -.5*(n-1)*log(g2*ymy + g1*yty);
mstart=molddraw;
lprobstart=lprobold;
inccount=zeros(bigk,1);
msize=0;
%I am keeping records for top 10 drawn models
%Here initialize this to initial model
top10mod=[molddraw molddraw molddraw molddraw molddraw ...
       molddraw molddraw molddraw molddraw molddraw];
lprobtop10=lprobold*ones(10,1);
top10count=zeros(10,1);

%calculate first and second moment of all coefficients
%Initialize them here
b1mo=zeros(bigk,1);
b2mo=zeros(bigk,1);
%Number of burnin and kept draws
nburn=100;
nkeep=1000;

nrep=nburn+nkeep;
for irep=1:nrep
   irep

   %choose at random on of the bigk potential explanatory variables
   %if it is already in the model, delete it else add it
   %Based on this, make up candidate model
indch=round(bigk*rand);
xnew=xold;
mnewdraw=molddraw;
if indch>0
if molddraw(indch,1)==1
     isum=0; 
    for i=1:indch
        isum=isum+molddraw(i,1);
    end
    xnew = [xold(:,1:isum) xold(:,isum+2:kold)];
    mnewdraw(indch,1)=0;
    
else
    isum=0; 
    for i=1:indch
        isum=isum+molddraw(i,1);
    end
    xnew = [xold(:,1:isum+1) xraw(:,indch) xold(:,isum+2:kold)];
    mnewdraw(indch,1)=1;
end
end

knew=sum(mnewdraw)+1;
xtxinv=inv(xnew'*xnew);
ymy = y'*y - y'*xnew*xtxinv*xnew'*y;
lprobnew = .5*knew*log(g1) -.5*(n-1)*log(g2*ymy + g1*yty); 

%Now decide whether to accept candidate draw
if log(rand) < (lprobnew - lprobold)
    xold=xnew;
    lprobold=lprobnew;
    molddraw=mnewdraw;
    kold=knew;
end

if irep>nburn
    %If new drawn model better than current top 10, add it to list
    for i=1:10
if lprobold>=lprobtop10(i,1)
  if sum(abs(molddraw - top10mod(:,i)))<.09
      break
  end
    if i<10
       lprobtop10(i+1:10,1)=lprobtop10(i:9,1);
       top10mod(:,i+1:10) = top10mod(:,i:9);
       top10count(i+1:10,1)=top10count(i:9,1);
    end
    lprobtop10(i,1)=lprobold;
    top10mod(:,i)=molddraw;
    top10count(i,1)=0;
    break
  end
 
end

   for i=1:10
       temp1=sum(abs(molddraw-top10mod(:,i)));       
       if temp1<.01
           top10count(i,1)=top10count(i,1)+1;
           break
       end
   end

    inccount = inccount + molddraw;
    msize=msize + kold;
    %calculating posterior properties of coefficients means
    %we have to write out full posterior
    Q1inv = (1+g0)*xold'*xold;
    Q0inv=g0*xold'*xold;
        Q1=inv(Q1inv);
        b1= Q1*xold'*y;
        vs2 = (y-xold*b1)'*(y-xold*b1) + b1'*Q0inv*b1;
        bcov = (vs2/(n-2))*Q1;
        %the next bit of this is awkward, needed to find out if variable is 
        %included in the model and, if so, to find out where it is
           summer1=1;
        for i=1:bigk
            bc=zeros(1,kold);        
            if molddraw(i,1)==1
              summer1=summer1+1;
              bc(1,summer1)=1;
              bmean=bc*b1;
            bvar =bc*bcov*bc';
          
            b1mo(i,1)=b1mo(i,1) + bmean;
            b2mo(i,1)=b2mo(i,1) + (bvar+bmean^2);
          end
      end
       
end

end

inccount=inccount./nkeep;
'proportion of models visited containing each variable'
varcount=cumsum(ones(bigk,1));
[varcount inccount]
'mean number of regressors in models'
msize/nkeep

'Prob of top 10 models, analytical (after deleting rest of models)'
lt1=lprobtop10 - max(lprobtop10);
exp(lt1)/sum(exp(lt1))

'Prob of top 10 models, numerical (after deleting rest of models)'
top10count./sum(top10count)

'Prob of top 10 models out of total number of models, numerical'
sum(top10count)/nkeep

 b1mo=b1mo./nkeep;
 b2mo=b2mo./nkeep;
 bsd=sqrt(b2mo-b1mo.^2);
 'Posterior mean and stand dev of each coefficient'
 [b1mo bsd]

