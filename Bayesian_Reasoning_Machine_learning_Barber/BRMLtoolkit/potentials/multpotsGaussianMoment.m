function newpot=multpotsGaussianMoment(pot1,pot2)
newpot=struct('variables',[],'table',[]);
v1=pot1.variables;
v2=pot2.variables;
[v ns]=potvariables([pot1 pot2]);
[dum vind1]=ismember(v1,v);
[dum vind2]=ismember(v2,v);
newdim(vind1)=pot1.table.dim;
ind1=getdimind(ns,vind1);

newdim(vind2)=pot2.table.dim;
ind2=getdimind(ns,vind2);

n=sum(newdim);

m1=pot1.table.mean;
m2=pot2.table.mean;
cov1=pot1.table.covariance;
cov2=pot2.table.covariance;
mm1=zeros(n,1); mm2=zeros(n,1); invcc1=zeros(n,n); invcc2=zeros(n,n);
mm1(ind1)=m1; mm2(ind2)=m2;
invcc1(ind1,ind1)=inv(cov1);invcc2(ind2,ind2)=inv(cov2);
invS=invcc1*inv(invcc1+invcc2)*invcc2; 
C=inv(invcc1+invcc2);
m=C*(invcc2*mm2+invcc1*mm1);
logc=-0.5*(mm1-mm2)'*invS*(mm1-mm2) +0.5*logdet(invS)-0.5*logdet(2*pi*eye(n));
newpot.table.mean=m;
newpot.table.covariance=C;
newpot.table.logprefactor=logc+pot1.table.logprefactor+pot2.table.logprefactor;
newpot.table.type='GaussianMoment';
newpot.variables=v;
newpot.table.dim=newdim;