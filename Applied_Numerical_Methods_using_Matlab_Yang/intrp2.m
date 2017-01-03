function Zi=intrp2(x,y,Z,xi,yi)
%To interpolate Z(x,y) on (xi,yi)
M=length(x); N=length(y);
Mi=length(xi); Ni=length(yi);
for mi=1:Mi
 for ni=1:Ni
  for m=2:M
   for n=2:N
     break1=0;
     if xi(mi)<=x(m)&yi(ni)<=y(n)
       tmp=(x(m)-xi(mi))*(y(n)-yi(ni))*Z(n-1,m-1)...
            +(xi(mi)-x(m-1))*(y(n)-yi(ni))*Z(n-1,m)...
            +(x(m)-xi(mi))*(yi(ni)-y(n-1))*Z(n,m-1)...
            +(xi(mi)-x(m-1))*(yi(ni)-y(n-1))*Z(n,m);
       Zi(ni,mi)=tmp/(x(m)-x(m-1))/(y(n)-y(n-1));
       break1=1;
     end
     if break1>0  break,  end
   end
   if break1>0  break,  end
  end
 end
end
