function H=gain_f(b,a,freq)
% H=gain_f(b,a,freq)
%
%Computes the gain of a digital filter at one or more frequencies.
%inputs:  b =coefficients of B(z)=b(1)*z^(-0)+ ... +b(N)*z^(-N+1)
%         a =coefficients of A(z)=a(1)*z^(-0)+ ... +a(M)*z^(-M+1)
%      freq =frequencies (Hz-s) at which to compute the gain
%
%output:  H =complex gain, B(z)/A(z), at frequencies given by freq.
%
%NOTE 1: If b(1:N) and a(1:M) are vectors, the filter is in direct form.
%        If b(Ns,N) and a(Ns,M) are arrays, the filter is in cascade form
%        with Ns sections.  The section weights are in rows.
%
%NOTE 2: If the frequencies are evenly spaced, the chirp-z transform is
%        used for faster computation.
%
% See also gain, gain_a, power_gain, pds

% Check for errors
[Ns,M]=size(a);
if M==1
    a=a(:)';                                %make a a row vector
    [Ns,M]=size(a);                         %Ns = # sections in cascade
end
[Nsb,N]=size(b);
if N==1
    b=b(:)';                                %make b a row vector
    [Nsb,N]=size(b);
end
if Ns~=Nsb,
    error('# rows (sections) in a and b must be the same')
elseif min(freq)<0 | max(freq)>.5
    error('frequencies are outside of [0, 0.5].')
end
% Inintialize
H=1;                                        %initialize the gain
n=0:N-1;                                    %indices in b
m=0:M-1;                                    %indices in a
v=freq(:)';                                 %=row vector of freqs (Hz-s)

%check to see if frequencies are equally spaced.
Nf=length(v);                               %# frequencies
v1=sort(v);                                 %v in ascending order
dv=v1(2:Nf)-v1(1:Nf-1);                     %spacings in v
maxdev=max(dv)-min(dv);                     %max. deviation in spacings

% Compute the cascade gain.
Eb=exp(-2*pi*j*(n'*v));
Ea=exp(-2*pi*j*(m'*v));
for sec=1:Ns,
    if maxdev<(v1(Nf)-v1(1))/1.e9           %test for equal spacing
        B=chirp_z(b(sec,:),v1(1),v1(Nf),Nf)';%section gain using chirp_z
        A=chirp_z(a(sec,:),v1(1),v1(Nf),Nf)';
    else
        B=b(sec,:)*Eb;                      %section gain using DFT
        A=a(sec,:)*Ea;
    end
if(min(abs(A))==0),
    error('Gain is infinite at some point.');
end
    H=H.*B./A;
end
