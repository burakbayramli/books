function [ty,y]=resamp_r(tx,x,T,ip)
%[ty,y]=resamp_r(tx,x,T,ip)
%
%This function produces a resampled version of a randomly-sampled waveform.
%
%Inputs:  tx =sample times (s) corresponding with elements in x. 
%             Times can be random; e.g., [1,6,-.2,3.4,...]
%         x  =samples of waveform x(t) at various times given by tx
%         T  =time step to use in resampling x
%         ip = 1 for printing M, cond, rel. error; 
%            = 0 or omitted for no printing
%
%Outputs: ty =times corresponding with the elements of y; time step = T
%         y  =resampled version of x with samples spaced T seconds apart
%
%The first sample of y is at min(tx). 
%The last sample is at or just before max(tx).
%
%See also: reconst, resamp_log, resamp, resamp_m

%parameters
cndmax=10;                      	%max. condition of G matrix
N0=length(x);                       %length of x
Mmax=200;                        	%max # sine components

%check for errors and set ip
if length(tx) ~=N0
    error('Vectors tx and x must be of equal length.')
elseif T > (max(tx)-min(tx))/4;
    error('T is too large considering the range of tx.')
elseif nargin<4
    ip=0;
end

%order the vectors tx and x
[tx,k]=sort(tx(:));
x=x(k);
x=x(:);

%if x is too long, use segments of tx and x
Ns=ceil(N0/Mmax);                           %# segments
Ls=ceil(N0/Ns);                             %length of all but last seg
ty=0;
y=0;
for ns=1:Ns
    k0=Ls*(ns-1)+1;                      	%start of current seg
    k1=min(N0,k0+Ls-1);                     %end of current seg
    t1=tx(k0:k1);
    x1=x(k0:k1);
    N1=k1-k0+1;                             %length of current seg
    
%if ns>1, append the last x7 sample to the start of x1
    if ns>1
        t1=[t7(N67);t1];
        x1=[x7(N67);x1];
        N1=N1+1;
    end

%remove a ramp from x1(1) to x1(N1)
    dxdt=(x1(N1)-x1(1))/(t1(N1)-t1(1));   	%slope of ramp
    r1=x1(1)+dxdt*(t1-t1(1));              	%r1 =ramp from x1(1) to x1(N1)
    x2=x1-r1;                             	%x2 =x1 with ramp removed
    t2=t1-t1(1);                        	%t2 begins at zero

%create an odd function using x2, with continuous first derivative
    x3=[x2;-x2(N1-1:-1:2)];                	%length of x3 & t3 is 2N1-2
    t3=[t2;2*t2(N1)-t2(N1-1:-1:2)];         %t3 begins at 0
    P3=2*t2(N1);                           	%P3 =period of x3

%fit x4 to x3 using M coefficients, with M recuced for cond. of matrix
    M1=min(N1,Mmax);
    arg=2*pi*t3/P3;                       	%args of G terms
    G1=sin(arg*(1:M1));                  	%max. G in equ. 2.11 of text
    M=M1+1;                                	%M will be reduced at least 1
    cnd=cndmax+1;
    while M>0 & cnd>cndmax              	%reduce M if necessary
        M=M-1;
        G=G1(:,1:M);                      	%G in equ. 2.11 of text
        cnd=cond(G'*G);                    	%condition of MxM array
    end
    c=pinv(G'*G)*(x3'*G)';               	%c in equ. 2.13, using pinv
    x4=c(1:M)'*sin(2*pi*(1:M)'*t3(1:N1)'/P3);
    x5=x4'+r1;                          	%restore the ramp
    re=sqrt(mean((x5-x1).^2))/max(abs(x1)); %relative error
    if ip==1
        fprintf('ns=%3.0f; M=%3.0f; cnd=%4.1f; re=%11.3e\n',ns,M,cnd,re)
    end

%compute x6 = N6 samples of x5 at times given by t6
    N67=fix((t1(N1)-t1(1))/T)+1;                 %length of x6 and x7
    t6=(0:N67-1)'*T;
    x6=c(1:M)'*sin(2*pi*(1:M)'*t6'/P3);
    r6=linspace(x1(1),x1(N1),N67)';
    t7=t1(1)+t6;
    x7=x6'+r6;
    if ns==1
        ty=t7;
        y=x7;
    else
        ty=[ty; t7];
        y=[y; x7];
    end
end
return

%plot x and y
sp_fig(2);
plot(tx,x,'*',ty,y,'-o'); grid;
legend('original','resampled')
