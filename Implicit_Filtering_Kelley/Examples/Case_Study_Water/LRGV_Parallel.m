function [fa, ifaila, icounta] = LRGV_Parallel(x,nsims)
[nx,mx]=size(x);
fa=zeros(1,mx);
parfor ip=1:mx
    z=x(:,ip);
    [ft, ift, ict] = LRGV_Cost(z,nsims);
    fa(ip)=ft;
    ifaila(ip)=ift;
    icounta(ip)=ict;
end
