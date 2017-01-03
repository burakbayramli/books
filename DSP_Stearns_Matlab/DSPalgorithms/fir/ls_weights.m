function b=ls_weights(f,d,N,c)
%b=ls_weights(f,d,N,c)
%
%Weights of LS FIR filter in text Fig. 8.5.
%
%Inputs: f =signal vector; input to LMS filter
%        d =desired output vector of LMS filter
%        N =number of weights (length of vector b)
%        c =0 (use covariance functions)
%          =1 (use correlation functions w/ zero extension)
%          =2 (use correlation functions w/ periodic extension)
%          =3 (linear phase; correlation w/periodic extension)
%          Note: if c=3 and N is even, N will be increased to N+1.
%
%Output: b =least squares weight vector

%check for errors.
if isvector(f)==0 | isvector(d)==0 | isscalar(N)==0,
    error('Arguments f and d must be vectors, and N must be a scalar.');
end
if c~=0 & c~=1 & c~=2 & c~=3,
    error('4th parameter (c) must be 0,1,2, or 3.');
end
f=col_vec(f); d=col_vec(d);

%covariance
if c==0,
    Rff=autocovar_mat(f,N);
    if cond(Rff)>1.e8,
        error('Singular autocovar. matrix - try reducing N.');
    end
    rfd=crosscovar(f,d,N);
    b=Rff\rfd;

%correlation using periodic extension
elseif c==1 | c==2,
    Pff=autocorr_mat(f,1,N);
    if cond(Pff)>1.e8,
        error('Singular autocorr. matrix - try reducing N.');
    end
    type=c-1;
    pfd=crosscorr(f,d,type,N);
    b=Pff\pfd;
    
%linear phase; correlation using periodic extension
elseif c==3,
    N=2*fix(N/2)+1;                 %assure N is odd
    L=fix(N/2);                     %L=(N-1)/2
    Pff1=autocorr_mat(f,1,L+1);     %1st matrix
    I2=ones(L+1,1)*[2*L:-1:L];      %matrix w/ [2L,2L-1,...,L] on each row
    I2=I2-[0:L]'*ones(1,L+1);       %index matrix for Pff2
    pff2=autocorr(f,1,N);           %complete autocorrelation function
    Pff2=pff2(I2+1);                %2nd matrix
    if cond(Pff1+Pff2)>1.e8,
        error('Singular autocorr. matrix - try reducing N.');
    end
    pfd=crosscorr(f,d,1,N);         %complete crosscorrelation vector
    pfd1=pfd(1:L+1);                %1st vector
    pfd2=pfd(N:-1:L+1);             %2nd vector
    c=(Pff1+Pff2)\(pfd1+pfd2);      %solution for c
    b=[c(1:L);2*c(L+1);c(L:-1:1)]/2;%solution for b
end
