function w=sqrtflt(w,x,perr,s,gamma,N)

%	A simple square root RLS adaptive filter			
%	For details, see:						
%	Digital Signal Processing: A Practical Approach			
%	E C Ifeachor and B W Jervis, Addison-Wesley, 1993		

forgt=sqrt(gamma);
sig=forgt;
sigsq=forgt*forgt;
ij=1; ji=1;
for j=2:N
	fj=0.0;
	for i=1:j-1
		 ji=ji+1;
		fj=fj+s(ji)*x(i);
	end
	a=sig/forgt;
	b=fj/sigsq;
	sigsq=sigsq+fj*fj;
	sig=sqrt(sigsq);
	a=a/sig;
	g(j)=s(ji)*fj;
	s(ji)=a*s(ji);
	for i=1:j-1
		ij=ij+1;
		sqp=s(ij);
		s(ij)=a*(sqp-b*g(i));
		g(i)=g(i)+sqp*fj;
	end
	ij=ij+1;
end
w = w + g'*perr/sigsq;