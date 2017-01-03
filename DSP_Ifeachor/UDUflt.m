function w=uduflt(w,x,u,ek,gamma,N)
%	udu algorithm - a numerically stable form of 			
%	the recursive least squares algorithm				
%									
%	inputs:								
%	x()	input vector                                          	
%	dn	latest input data value					
%	w()	coefficient vector                                   	
%	u()   	vector containing elements of U and D			
%									
%	outputs:							
%	en	error signal						
%	yn	digital filter output					
%	w()	updated coefficient vector				
%	u()	updated elements of U and D				
%                                                         		

sf = 1/gamma;

m=1;                     	% update the UD elements 
v=zeros(1,N);
v(1)=x(1);
for j=2:N
	v(j)=x(j);
	for k=1:j-1
		m=m+1;
		v(j)=v(j)+u(m)*x(k);
	end
	m=m+1;
	b(j)=u(m)*v(j);
end
b(1)=u(1)*x(1);
alpha=gamma+b(1)*v(1);
delta=1/alpha;
u(1)=u(1)*delta;

m=1;
for j=2:N 
	beta1=alpha;
	alpha=alpha+b(j)*v(j);
	p=-v(j)*delta;
	delta=1/alpha;
	for k=1:j-1
		m=m+1;
		beta=u(m);
		u(m)=beta+b(k)*p;
		b(k)=b(k)+b(j)*beta;
	end
	m=m+1;
	u(m)=u(m)*beta1*delta*sf;
end
perr=ek/alpha;
for j=1:N 		% update the weights 
	w(j)=w(j)+b(j)*perr;
end