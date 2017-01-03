% Worsheet Cumulative angular function
function  AngFuncWrst(n,scale) 

% Scale and position
  a0=128; b0=128;
  d=50;
  m=100;        % num puntos
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Function 
	p=0:2*pi/m:2*pi-pi/m;             % Parameter
%	X=a0+d*(cos(p)+.1*cos(5*p)+.1*cos(7*p));
%	Y=b0+d*(sin(p)-.1*sin(5*p)+.1*sin(7*p));

	X=a0+d*(cos(p));
	Y=b0+d*(sin(p));
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% Arc lenght
	S=zeros(1,m);
	S(1)=sqrt( (X(1)-X(m))^2 + (Y(1)-Y(m))^2 );
  for i=2:m
    S(i)=S(i-1)+sqrt( (X(i)-X(i-1))^2 + (Y(i)-Y(i-1))^2 );
  end
  L=S(m);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% Normalised Parameter 
  t=(2*pi*S)/L;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Graph of the curve 
 	subplot(3,3,1);							% The plot
 	plot(X,Y);                      
 	axis([0,250,0,250]);        % Axis of the graph pf the curve
 	axis square;                % Aspect ratio
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 	
% Graph of the angular function  y'/x'
  avrg=1;
  A=zeros(1,m);
  for i=1:m
    x1=0; x2=0; y1=0; y2=0;
    for j=1:avrg
       pa=i-j; pb=i+j;
       if(pa<1) pa=m+pa; end
       if(pb>m) pb=pb-m; end
       x1=x1+X(pa); y1=y1+Y(pa);
     x2=x2+X(pb); y2=y2+Y(pb);
    end
    x1=x1/avrg; y1=y1/avrg;
    x2=x2/avrg; y2=y2/avrg;
    
  	dx=x2-x1;
  	dy=y2-y1;

  	if(dx==0) dx=.000001; end 
  	if dx>0 & dy>0
  	  A(i)=atan(dy/dx);
  	elseif dx>0 & dy<0
  		A(i)=atan(dy/dx)+2*pi;
  	else
  		A(i)=atan(dy/dx)+pi;
  	end	
  end

  subplot(3,3,2);						  % The plot
  plot(S,A);
  axis([0,S(m),-1,2*pi+1]);     % Axis of the graph of the angular curve
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
% Cummulative angular   G(s)=-2pi
  G=zeros(1,m);
  for i=2:m
    d=min(abs(A(i)-A(i-1)),abs(abs(A(i)-A(i-1))-2*pi));
       
    if d>.5 
       G(i)=G(i-1);      
    elseif (A(i) -A(i-1))<-pi
       G(i)=G(i-1)-(A(i) -A(i-1)+2*pi); 
    elseif (A(i) -A(i-1))>pi
       G(i)=G(i-1)-(A(i) -A(i-1)-2*pi);  
    else
       G(i)=G(i-1)-(A(i) -A(i-1)); 
    end
  
  end         
       
  subplot(3,3,3);						  % The plot
  plot(S,G);                  
  axis([0,S(m),-2*pi-1,1]);     % Axis of the graph pf the curve
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
% Cummulative angular Normalised
  F=G+t;
 
  subplot(3,3,4);						  % The plot
  plot(t,F);                  
  axis([0,2*pi,-2*pi,2*pi]);      % Axis of the graph pf the curve
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
  

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
% Fourier Descriptors
 
  if(n==0) n=floor(m/2); end; % num coeficients

	a=zeros(1,n);               % Fourier Coefficients
	b=zeros(1,n);

	for k=1:n
	    a(k)=a(k)+G(1)*(S(1))*cos(2*pi*k*S(1)/L);
     	b(k)=b(k)+G(1)*(S(1))*sin(2*pi*k*S(1)/L);
   	for i=2:m
    	a(k)=a(k)+G(i)*(S(i)-S(i-1))*cos(2*pi*k*S(i)/L);
     	b(k)=b(k)+G(i)*(S(i)-S(i-1))*sin(2*pi*k*S(i)/L);
 	  end
  	a(k)=a(k)*(2/L);
   	b(k)=b(k)*(2/L)-2/k;
	end

  subplot(3,3,7);						  
  bar(a);
  axis([0,n,-scale,scale]);  
  
  subplot(3,3,8);						
  bar(b);
  axis([0,n,-scale,scale]);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
% Rotation invariant Fourier Descriptors
	CA=zeros(1,n);
	for k=1:n  
	  CA(k)=sqrt(a(k)^2+b(k)^2);
	end
	
  subplot(3,3,9);						  % The plot
  bar(CA);
  axis([0,n,-scale,scale]);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
  
