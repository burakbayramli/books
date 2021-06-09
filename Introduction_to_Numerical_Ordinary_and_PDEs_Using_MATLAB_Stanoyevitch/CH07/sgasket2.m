function [ ] = sgasket2(V1,V2,V3,niter)
%input variables: V1,V2,V3 are vertices of an equilateral triangle in 
%the plane taken as row vectors, niter is the number of iterations
%used to obtain points in the fractal.  The output will be a plot of
%all of the points.  If niter is not specified, the default value 
%of 5000 is used.  

%if only 3 input arguments are given (nargin==3), set niter to default if nargin == 3, niter = 5000; end

%Similitude matrices for Sierpinski gasket. 
S1=[.5 0 0;0 .5 0;0 0 1];
S2=[.5 0 (V2(1)-V1(1))/2; 0 .5 (V2(2)-V1(2))/2;0 0 1];
S3=[.5 0 (V3(1)-V1(1))/2; 0 .5 (V3(2)-V1(2))/2;0 0 1]; 

%Probability vector for Sierpinski gasket has equal probabilities (1/3)
%for choosing one of the three similitudes.  
P = [1/3 2/3];

%prepare graphics window for repeated plots of points
clf, axis('equal'); hold on;

%introduce "floating point" (can be any vertex) in homogeneous %coordinates
Float=[V1(1);V1(2);1];
i = 1; %initialize iteration counter

%Begin iteration for creating new floating points and plotting each one %that arises.
while i <= niter
   choice = rand;    
   if choice < P(1);        
      New = S1 * Float;       
      plot (New(1), New(2));     
   elseif choice < P(2);        
      New = S2 * Float;        
      plot (New(1), New(2));      
   else       New = S3 * Float;      
      plot (New(1), New(2));      
   end;   
   Float=New;    i = i + 1;
end
hold off
