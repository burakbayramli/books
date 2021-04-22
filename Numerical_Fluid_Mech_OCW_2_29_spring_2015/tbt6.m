n=4
a = [ [1 1]' [1 1.0001]']
%b= [1 2]'
b= [1 0]'

ai=inv(a);
a_nrm=max( abs(a(1,1)) + abs(a(1,2)) , abs(a(2,1)) + abs(a(2,2)) )
ai_nrm=max( abs(ai(1,1)) + abs(ai(1,2)) , abs(ai(2,1)) + abs(ai(2,2)) )
k=a_nrm*ai_nrm

r=ai * b

x=[0 0];
m21=a(2,1)/a(1,1);
a(2,1)=0;
a(2,2) = radd(a(2,2),-m21*a(1,2),n);
b(2)   = radd(b(2),-m21*b(1),n);

x(2)   = b(2)/a(2,2);
x(1)   = (radd(b(1), -a(1,2)*x(2),n))/a(1,1);
x'


