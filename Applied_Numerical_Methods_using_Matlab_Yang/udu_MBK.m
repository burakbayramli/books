function [u,du]=udu_MBK(t)
%u= *mod(t,1);
i= fix(t); 
if mod(i,2)==0, u=t-i; du=1;
 else u=1-t+i; du=-1;
end   

%index=find(mod(i,2)==0);
%u(index)=t(index)-i(index); 
%du(index)=1; 
%index=find(mod(i,2)~=0);
%u(index)=1-t(index)+i(index); 
%du(index)=-1; 
