function H=gain_a(b,a,omega)
%H=gain_a(b,a,omega)
%
%Analog gain of H(s)=B(s)/A(s), 
%    where B(s)=b(1)*s^(N-1)+...+b(N), and A(s)=a(1)*s^(M-1)+...+a(M).
%
%Inputs:      b =numerator coeff. of H(s), b(1:N)
%             a =denominator coeff., a(1:M)
%         omega =frequency vector (rad/s)
%
%Output:      H=complex gain vector, H(omega)
%
%See also: gain, gain_f

%check for errors
if length(a)~=numel(a) | length(b)~=numel(b)
    error('b and a must be vectors');
end

%compute H
H=zeros(length(omega),1);
bexp=length(b)-1:-1:0;
aexp=length(a)-1:-1:0;
for m=1:length(omega)
    jw=(j*omega(m)).^bexp;
    B=jw*b(:);
    jw=(j*omega(m)).^aexp;
    A=jw*a(:);
    H(m)=B/A;
end
