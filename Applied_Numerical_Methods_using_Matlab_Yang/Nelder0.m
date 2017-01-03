function [xo,fo]=Nelder0(f,abc,fabc,TolX,TolFun,k)
[fabc,I]=sort(fabc); a=abc(I(1),:); b=abc(I(2),:); c=abc(I(3),:);
fa=fabc(1); fb=fabc(2); fc=fabc(3);  fba=fb-fa; fcb=fc-fb;
if k<=0|abs(fba)+abs(fcb)<TolFun|abs(b-a)+abs(c-b)<TolX
  xo=a; fo=fa; 
if k==0, fprintf('Just best in given # of iterations'), end
else
  m=(a+b)/2;  e=3*m-2*c;  fe=feval(f,e); 
  if fe<fb, c=e;  fc=fe;
   else  
r=(m+e)/2;  fr=feval(f,r); 
     if fr<fc,  c=r;  fc=fr;  end
     if fr>=fb
       s=(c+m)/2;  fs=feval(f,s);
       if fs<fc,  c=s;  fc=fs; 
        else b=m; c=(a+c)/2;  fb=feval(f,b); fc=feval(f,c);
       end    
     end 
  end
  [xo,fo]= Nelder0(f,[a;b;c],[fa fb fc],TolX,TolFun,k-1);  
end
