function [wn] = ncon_btbc(wa,dtdx,m)
  %
  % Non conservative scheme for the Burgers equation
  %
  wam=zeros(1,m+1);
  wan=zeros(1,m+1);
  cflw=zeros(1,m+1);
				%
  wam(1:m)=wa(2:m+1);
				% Transmissive boundary conditions
  wam(m+1)=wa(m);
				%
  wan(2:m+1)=wa(1:m);
				% Transmissive boundary conditions
  wan(1)=wa(2);
				%
  cflw(1:m+1)=dtdx.*wa(1:m+1);
				%
  for i=1:m+1
    if(wa(i)>=0)
      wn(i)=wa(i)-...
	    cflw(i)*( wa(i)-wan(i) );
    else
      wn(i)=wa(i)-...
	    cflw(i)*( wam(i)-wa(i) );
    end
  end    
