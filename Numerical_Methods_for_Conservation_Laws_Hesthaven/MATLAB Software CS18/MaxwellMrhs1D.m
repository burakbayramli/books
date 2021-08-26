function [dEH] = MaxwellMrhs1D(x,EM,ep,mu,h,k,maxvel)
% function [dEH] = MaxwellMrhs1D(x,EM,ep,mu,h,k,maxvel);
% Purpose: Evaluate right hand side for Maxwell's equations 
% using a monotone method
N = length(x);

% PEC boundary conditions by mirrow principle
[xe,Ee] = extend(x,EM(:,1),h,1,'D',0,'D',0); 
[xe,He] = extend(x,EM(:,2),h,1,'N',0,'N',0); 
EH = [Ee(2:N+1) He(2:N+1)]; 
EHp = [Ee(3:N+2) He(3:N+2)]; EHm = [Ee(1:N) He(1:N)];

% Change numerical flux here
dEH = - (MaxwellLF(EM,EHp,ep,mu,k/h,maxvel) - ...
            MaxwellLF(EHm,EM,ep,mu,k/h,maxvel))/h;
return