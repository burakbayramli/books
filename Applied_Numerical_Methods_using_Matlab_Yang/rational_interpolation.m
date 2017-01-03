function yi=rational_interpolation(x,y,xi)
N=length(x); Ni=length(xi);
R(:,1)=y(:);
for n=1:Ni
  xn=xi(n);
 for i=1:N-1
     for m=1:N-i
        RR1= R(m+1,i); RR2= R(m,i);
        if i>1, 
RR1= RR1-R(m+1,???); RR2= RR2-R(???,i-1);
end
tmp1= (xn-x(???))*RR1;
num= tmp1*(R(???,i)-R(m,?));
den= (xn-x(?))*RR2 -tmp1;
R(m,i+1)= R(m+1,i) ????????; 
 end
end 
  yi(n)= R(1,N);
end
