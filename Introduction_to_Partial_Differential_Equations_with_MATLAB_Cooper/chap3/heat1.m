%                          program heat1
%
%      This program plots a Riemann sum which approximates the
%   convolution of the heat kernal with a function ff(x). The
%   Riemann sum uses the midpoint rule.  
%      The initial data is written into the vector snap0 and the
%   approximate solution at time t is written into the vector snap1.
%      At run time, user must enter the diffusion coefficient k,
%   the time t > 0 of the snapshot, and the number of terms n (must be even)
%   in the Riemann sum. The program plots snap0 together with
%   snap1  on the interval [-10, 10].  
%      The program also computes the Gaussian approximation at time t.
%   To compare the solution snap1 with the Gaussian approximation
%   use   plot(x,snap1,x,gauss).
%
%      There are two choices of initial data.  The first choice is
%   a step function
%
%                         f(x) = -1 for -1 < x < 0
%                         f(x) =  2 for 0 < x < 1
%   This function is "built in" to the program. If you wish to
%   have other initial data, you must provide an mfile ff.m or an 
%   inline function ff = inline . . . . 
%   Even if the function ff is defined for all x, the 
%   program heat1 only uses the values of ff for -1 < x < 1, so that it is
%   equivalent to setting the function to zero outside the interval
%   [-1,1].
 


disp( '   ')
k = input('enter the value of the diffusion constant k  ')
t = input('enter the value of the time t to view solution  ')
n = input('enter the number of points y(j)  ')

dely = 2.0/n;
delx = .01;
x = -10:delx:10;
y = -1 +.5*dely :dely :1 -.5*dely;


disp(' Is the initial data the given step function ?  ')
mm = input('Enter 1 if yes,  0 if not   ');

if mm == 1

    snap0 = (x < -1) -(x < 0) + 2*( ( x < 1) - (x < 0) );

    s = zeros(size(x));

    for i = 1:(n/2)
       s  = s - exp(-(x-y(i)).^2/(4*k*t));
    end

    for i =  n/2 +1 : n
       s = s + 2*exp(-(x-y(i)).^2/(4*k*t));
    end

    snap1 = (1.0/sqrt(4*pi*k*t))*s*dely;

    Q = 1
    mean = 1.5
    p0=   - 1.9167
    gauss = Q*(1-p0/(4*k*t))*exp(-(x-mean).^2/(4*k*t) )/sqrt(4*pi*k*t);
else

    snap0 = ( (x < 1) - (x < -1) ).*ff(x);

    s = zeros(size(x));
    
    for i = 1:n
       s = s + exp(-(x-y(i)).^2/(4*k*t))*ff(y(i));
    end

    snap1 = (1.0/sqrt(4*k*pi*t))*s*dely;
    
    simp = 2*ones(1,201);
    simp(2:2:200) = 4*ones(100,1);
    simp(1) = 1; simp(201) = 1;
    xx = linspace(-1,1, 201)';
    funval = ff(xx);
    Q = (1/300)*simp*funval

      if abs(Q) < eps
          disp(' The quantity Q is too close to zero; program cannot calculate the gaussian approximation ') 
          plot(x,snap0, x,snap1)
      else
         
          mean = (1/300)*simp*(xx.*funval)/Q
          p0 = (1/300)*simp*(xx.^2.*funval) - mean^2 

          gauss = Q*(1-p0/(4*k*t))*exp(-(x-mean).^2/(4*k*t) )/sqrt(4*pi*k*t);
       end
   end

 plot(x,snap0,x,snap1)
