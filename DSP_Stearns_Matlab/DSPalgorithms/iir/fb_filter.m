function [y,v]=fb_filter(A,b1,a1,b2,a2,x)
%[y,v]=fb_filter(A,b1,a1,b2,a2,x)
%
%Same as "filter" but with H(z)=A*H1(z)/(1+H1(z)*H2(z)).
%
%A is a constant gain coeffient
%b1 and a1 are the weight vectors of H1(z)
%b2 and a2 are the weight vectors of H2(z)
%x is the input signal vector
%
%y is the output sigmal vector
%v is the feedback vector subtracted from x; i.e., the output of H2(z)

b1=b1(:)'; a1=a1(:)'; b2=b2(:)'; a2=a2(:)'; %row weight vectors

%equation (2) in memo:
N1D2=conv(b1,a2);
D1D2=conv(a1,a2);
N1N2=conv(b1,b2);
bd=N1D2;                            %numerator of H(s)in (2)
ad=poly_sum(D1D2,N1N2);             %denominator of H(s) in (2)
y=A*filter(bd,ad,x);
v=filter(b2,a2,y);
