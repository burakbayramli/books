function x=iwavelet1(y,Nwts,win)
% x=iwavelet1(y,Nwts,win)
%
% Inverse wavelet transform - recovers wavelet1 input vector.
% 
% Inputs:
%	 y     =down-sampled column vectors - output of wavelet1. 
%           y(:,1)=low freq., Y(:,2)=next, etc.
%           y has Ns columns, where Ns=2^(# stages in wavelet trans.).
%   Nwts   =number of weights used in execution of qmf.
%   win    =window used in qmf.
%
% Output:
%   x = recovered signal (input to wavtraqns1) in a column vector.

% See also: iwavelet1, wavelet2, iwavelet2, qmf, iqmf

% Check for errors.
[M,Ns]=size(y);                         %Ns =# cols (signal bands) in y
Nstgs=fix(log2(Ns));                    %Nstgs =# stages in decomposition
if Ns~=2^Nstgs,
    error('Input error: # columns in y must be a power of 2.');
end

L=M;                                    %L =stage signal length
u=y;                                    %u is initially y
for stg=Nstgs:-1:1,                     %for stages Nstgs,...,2,1
    Nf=2^(stg-1);                       %Nf =# iqmf's in this stage
    x=zeros(2*L,Nf);                    %x will =recovered stage input
    for i=1:Nf,
        c=2*i-1:2*i;                    %c =2 cols. in stage output
        if 2*fix(i/2)==i,
            u(:,c)=u(:,rev(c));         %if i is even, reverse cols. of u
        end
        x(:,i)=iqmf(u(:,c),Nwts,win);   %x =ith col. in stage input
    end
    L=2*L;                              %L =twice previous L
    u=x;
end
