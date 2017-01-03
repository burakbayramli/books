function yi=intrp1(x,y,xi)
M=length(x); Mi=length(xi); 
for mi=1: Mi
  if xi(mi)<x(1), yi(mi)=y(1)-(y(2)-y(1))/(x(2)-x(1))*(x(1)-xi(mi));
  elseif  xi(mi)>x(M)
     yi(mi)=y(M)+(y(M)-y(M-1))/(x(M)-x(M-1))*(xi(mi)-x(M));
   else
     for m=2:M
       if xi(mi)<=x(m)
         yi(mi)=y(m-1)+(y(m)-y(m-1))/(x(m)-x(m-1))*(xi(mi)-x(m-1));
         break;
       end
     end
  end
end