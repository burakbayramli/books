function newpot=multpotsGaussianCanonical(pot1,pot2)
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
    
invcov1=pot1.table.invcovariance;
invcov2=pot2.table.invcovariance;
invm1=pot1.table.invmean;
invm2=pot2.table.invmean;
invmm1=zeros(n,1); invmm2=zeros(n,1); invcc1=zeros(n,n); invcc2=zeros(n,n);
invmm1(ind1)=invm1; invmm2(ind2)=invm2;
invcc1(ind1,ind1)=invcov1;invcc2(ind2,ind2)=invcov2;

invC=invcc1+invcc2;
invm=invmm2+invmm1;
newpot.table.invmean=invm;
newpot.table.invcovariance=invC;
newpot.table.logprefactor=pot1.table.logprefactor+pot2.table.logprefactor;
newpot.table.type='GaussianCanonical';
newpot.variables=v;
newpot.table.dim=newdim;