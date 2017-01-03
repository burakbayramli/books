function dx=f_MBK(t,x)
M=1; B=0.1; K=0.1; %g=9.8;
[u,du]= udu_MBK(t);
dx=x*[0  1; -B/M -K/M]'+[0 (K*u+B*du)/M];
%dx=x*[0  1; -B/M -K/M]'+[0 -g+(K*u+B*du)/M];
