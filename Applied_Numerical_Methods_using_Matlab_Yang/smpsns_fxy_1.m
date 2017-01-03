function INTf=smpsns_fxy(f,x,c,d,N)
.. .. .. .. .. .. .. .. .. .. ..
sum_odd=f(x,y(2));  sum_even=0;
for n=4:2:N
   sum_odd=sum_odd+f(x,y(n)); sum_even=sum_even+f(x,y(n-1));
end
INTf=(f(x,y(1))+f(x,y(N+1))+4*sum_odd+2*sum_even)*h/3;
.. .. .. .. .. .. .. .. .. .. ..
