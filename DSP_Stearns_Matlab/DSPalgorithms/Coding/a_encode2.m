function [y,Nsym,K]=a_encode2(x)
%[y,Nsym,K]=a_encode2(x)
%
%Adaptive arithmetic encoding of integer vector x with min(x)=0
% and max(x)<2^8.
% All the symbol frequencies are initially assumed to be equally likely.
%
%Input:   x =integer vector with min(x)=0 and max(x)<=255.
%
%Outputs: y =uint*8 vector containing the encoded version of x.
%         Nsym =# symobls =max(x)+1; must be >=2 and <=2^8.
%         K =length(x).
%
%See also: a_decode2, e_encode2, e_decode2, a_encode, a_decode

%check the input
if min(x)~=0 | max(abs(x-fix(x)))~=0 | max(x)>=2^8,
    error('x must be integers with min(x)=0 and max(x)<2^8');
end
K=length(x);                    %K =#samples in x to encode

% Set word partitions.
W=44;                           % W =unsigned word size in bits
t=2^(W/2)-1;
t1=(t+1)/4;                     %t1-t3 =boundaries within W
t2=2*t1;
t3=3*t1;

% Initialize the frequency vector.
Nsym=max(x)+1;
if Nsym==1,                     %Nsym=1 means x is a vector of zeros
    y=0;
    return
end
f=ones(1,Nsym);                 %f =symbol freqencies, initialized to 1.
M=triu(ones(Nsym,Nsym));        %M =array used to compute cum. freqs.

% Initialize for encoding.
Nbb=8*2^10;                     %Nbb =capacity of buf (bits)
y=zeros(Nbb/8,1,'uint8');       %y is initially set to hold Nbb bits
buf=zeros(8,Nbb/8);             %bit buffer: 8 zeros in each column
bv=2.^[7:-1:0];                 %bv =vector used in emptying buffer
L=0;                            %L =lower bound in W
H=t;                            %H =upper bound in W
Nbits=0;                        %Nbits =#bits used in encoding
Nbtf=0;                         %Nbtf  =#bits to follow
Nbuf=0;                         %Nbuf  =#bits stored in buffer
Ny=0;                           %Ny    =#bytes stored in y

% Encode loop starts here. sym range updated to [1,N].
tic;
for ix=1:K;
%................Uncomment the next 3 lines to print timing...............
%    if rem(ix,50000)==0,
%        t=toc; fprintf('encode ix=%8.0f; t=%8.1f sec\n',ix,t); tic;
%    end
%.........................................................................
    sym=x(ix)+1;                %sym range is [1,256]
    cf=[0 f(1:Nsym)*M];         %update the cum. freqs based on f
    R=H-L;                      %update R,H, and L based on cf
    H=fix(L+(R*cf(sym+1))/cf(Nsym+1) - 1);
    L=fix(L+(R*cf(sym))/cf(Nsym+1));
    % Ensure L < t2 <= H, and (L,H) not in [t1,t3).
    while H<t2 | L>=t2 | (L>=t1 & H<t3),
        if H<t2
            Nbuf=Nbuf+1;                    %effectively output a zero
            if Nbtf>0,
                i=1:Nbtf;
                buf(Nbuf+i)=ones(Nbtf,1);   %install Nbtf ones
                Nbuf=Nbuf+Nbtf;             %last bit is at Nbuf
                Nbtf=0;                     %reset Nbtf
            end
        elseif L>=t2
            Nbuf=Nbuf+1;
            buf(Nbuf)=1;                    %output a one
            if Nbtf>0,
                Nbuf=Nbuf+Nbtf;             %effectively output Nbtf zeros
                Nbtf=0;                     %reset Nbtf
            end
            L=L-t2;
            H=H-t2;
        else
            Nbtf=Nbtf+1;
            L=L-t1;
            H=H-t1;
        end
        L=2*L;
        H=2*H+1;
    end
    f(sym)=f(sym)+1;                        %update f for next x
    if Nbuf>=Nbb/2,                         %if buffer is > half full, ...
        nB=Nbb/16;                          %# bytes in 1st half of buf
        nb=8*nB;                            %# bits in 1st half of buf
        y(Ny+1:Ny+nB)=bv*buf(:,1:nB);       %dump 1st half of buffer to y
        y=[y;zeros(nB,1)];                  %append nB zeros to y
        Ny=Ny+nB;                           %Ny =end of y
        z=zeros(1,Nbb-nb);                  %zeros to fill 2nd half of buf
        buf(:)=[buf(nb+1:Nbb),z];           %shift 1st half out of buf
        Nbuf=Nbuf-nb;                       %reset buffer index
    end
end
% End of encode loop. Append termination bits.
Nbtf=Nbtf+1;

if L<t1
    Nbuf=Nbuf+1;                            %effectively output a zero
            if Nbtf>0,
                i=1:Nbtf;
                buf(Nbuf+i)=ones(Nbtf,1);   %install Nbtf ones
                Nbuf=Nbuf+Nbtf;             %last bit is at Nbuf
            end
elseif L>=t1
            Nbuf=Nbuf+1;
            buf(Nbuf)=1;                    %output a one
    if Nbtf>0,
        Nbuf=Nbuf+Nbtf;                     %effectively output Nbtf zeros
    end
end
    if Nbuf>0,                              %if buffer is not empty, ...
        nB=ceil(Nbuf/8);                    %# bytes needed in y
        y(Ny+1:Ny+nB)=bv*buf(:,1:nB);       %dump 1st half of buffer to y
        Ny=Ny+nB;                           %Ny =end of y
    end
y=y(1:Ny);                                  %set length of y
