function c=poly_sum(c1,c2)
%c=poly_sum(c1,c2)
%
%creates the sum of 2 polymomials. The c's are coefficent vetors.
%For example,
%c1=[1 2 3]; c2=[4 5]; c=[1 6 8] corresponds with
%(x^2+2*x+3) + (4*x+5) = x^2+6*x+8

c1=c1(:)';                      %row vectors
c2=c2(:)';
L1=length(c1);                  %vector lengths
L2=length(c2);

if L1>L2
    c=c1+[zeros(1,L1-L2) c2];
elseif L2>L1
    c=c2+[zeros(1,L2-L1) c1];
else
    c=c1+c2;
end
