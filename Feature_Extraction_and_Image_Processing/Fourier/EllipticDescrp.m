% Elliptic Fourier Descriptors
function  EllipticDescrp(curve,n,scale)  % n= num coefficients
                                        % if n=0 then n=m/2 
                                        % Scale amplitud output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Function from image
X=curve(1,:);
Y=curve(2,:);
m=size(X,2);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Function (analytic)
% Parameters: Scale and position
%  a0=128; b0=128;
%  d=50;
%  m=100;        % num puntos
  
%	p=0:2*pi/m:2*pi-pi/m;             % Parameter
%	X=a0+d*(cos(p)+.3*cos(5*p)+.1*cos(7*p));
%	Y=b0+d*(sin(p)-.3*sin(5*p)+.1*sin(7*p));

%	X=a0+d*(cos(p));
%	Y=b0+d*(sin(p));
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Graph of the curve 
	subplot(3,3,1);							     % The plot
 	plot(X,Y);   
 	mx=max(max(X),max(Y))+10; 
 	axis([0,mx,0,mx]); % Axis of the graph pf the curve
 	axis square;                     % Aspect ratio
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Graph of X
  p=0:2*pi/m:2*pi-pi/m;             % Parameter
	subplot(3,3,2);							     % The plot
 	plot(p,X);                 
 	axis([0,2*pi,0,mx]); % Axis of the graph pf the curve
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Graph of Y
	subplot(3,3,3);							     % The plot
 	plot(p,Y);                 
 	axis([0,2*pi,0,mx]); % Axis of the graph pf the curve
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
% Elliptic Fourier Descriptors
 
  if(n==0) n=floor(m/2); end; % num coeficients

	ax=zeros(1,n);              % Fourier Coefficients
	bx=zeros(1,n);
	ay=zeros(1,n);
	by=zeros(1,n);
		
	t=2*pi/m;

	for k=1:n
   	for i=1:m
    	ax(k)=ax(k)+X(i)*cos(k*t*(i-1));
     	bx(k)=bx(k)+X(i)*sin(k*t*(i-1));
     	ay(k)=ay(k)+Y(i)*cos(k*t*(i-1));
     	by(k)=by(k)+Y(i)*sin(k*t*(i-1));   	
 	  end
  	ax(k)=ax(k)*(2/m);
   	bx(k)=bx(k)*(2/m);
  	ay(k)=ay(k)*(2/m);
   	by(k)=by(k)*(2/m);  	
	end
	

  subplot(3,3,4);						  
  bar(ax);
  axis([0,n,-scale,scale]);  
  
  subplot(3,3,5);						
  bar(ay);
  axis([0,n,-scale,scale]);
  
  subplot(3,3,6);						  
  bar(bx);
  axis([0,n,-scale,scale]);  
  
  subplot(3,3,7);						
  bar(by);
  axis([0,n,-scale,scale]);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
% Invariant
 CE=zeros(1,n);

 for k=1:n
  	CE(k)=sqrt((ax(k)^2+ay(k)^2)/(ax(1)^2+ay(1)^2))+sqrt((bx(k)^2+by(k)^2)/(bx(1)^2+by(1)^2));
	end

  subplot(3,3,8);						  
  bar(CE);
  axis([0,n,0,.6]);  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
% Reconstruction

	ax0=0;
	ay0=0;
	for i=1:m	
	    ax0=ax0+X(i);
     	ay0=ay0+Y(i);
  end
  ax0=ax0/m;
  ay0=ay0/m;

 RX=ones(1,m)*ax0;
 RY=ones(1,m)*ay0;

 for i=1:m
    for k=1:n
    	RX(i)=RX(i)+ax(k)*cos(k*t*(i-1))+bx(k)*sin(k*t*(i-1));
     	RY(i)=RY(i)+ay(k)*cos(k*t*(i-1))+by(k)*sin(k*t*(i-1));
 	  end	
	end
	
 subplot(3,3,9);		
 plot(RX,RY);   
 mx=max(max(RX),max(RY))+10; 
 axis([0,mx,0,mx]);            % Axis of the graph pf the curve
 axis square;                  % Aspect ratio
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
