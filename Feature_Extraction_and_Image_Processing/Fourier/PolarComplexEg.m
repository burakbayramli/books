% Example of Polar Complex Function
% 25 March 2000

function PolarComplexEg()


  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Function 
  m=100; a0=128; b0=128;d=50;
	t=0:2*pi/m:4*pi-pi/m;             % Parameter
  X=a0+d*(cos(t)+.1*cos(5*t)+.1*cos(7*t));
	Y=b0+d*(sin(t)-.1*sin(5*t)+.1*sin(7*t));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Graph of the curve 
 	subplot(3,3,1);							% The plot
 	plot(X,Y);                      
 	axis([0,250,0,250]);        % Axis of the graph pf the curve
 	axis square;                % Aspect ratio
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plot Each axes
	subplot(3,3,2);							% The plot
 	plot(t,X);                      
 	axis([0,4*pi,0,250]);        % Axis of the graph pf the curve
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plot Each axes
	subplot(3,3,3);							% The plot
 	plot(t,Y);                      
 	axis([0,4*pi,0,250]);        % Axis of the graph pf the curve
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
