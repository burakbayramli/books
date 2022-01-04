
%                       Program disper
%
%   This progam uses the (inverse) fast fourier transform to compute the
%   solutions of three different dispersive equations with the same
%   initial data u0(x).  The three equations are 
%
%   Linearized Klein-Gordon: u_tt -u_xx +u = 0
%   with dispersion relation omega1(k) = sqrt(1+k**2)
%
%   Linearized Korteweg deVries: u_t +10u_x - (1/3)u_xxx = 0
%   with dispersion relation omega2(k) = 10*k -(1/3)k**3
%
%   Vibrating Beam equation:  u_tt -u_xxxx = 0
%   with dispersion relation omega3(k) = k**2
%
%   The initial data for each is 
%
%      u0(x) = exp(-(x**2/24))*exp(2ix)
%   In the transform variable the initial data is
%
%        U(k) = exp(-6*(k-2)**2)*sqrt(24*pi)
%
%   The data is sampled on the interval  k in [0,K]. K is set to 64. 
%   The number of sample points is N (set to 4096), so delk = 64/4096 = .016.
%   The result is displayed on the interval x in [-pi/delk, pi/delk]
%   intervals delx = 2*pi/K.
%
%   The values of u(x,t) go into the vector u
%   The values of u(x,0) go into the vector u0
%   The values of u(x,0) translated to the right with speed
%   equal to the group velocity at k = 2 go into the vector v.
%   The latter can be used to compare how the wave is being
%   deformed by the dispersion process.
%
%   There are four supporting mfiles: bigu.m for initial data,
%   omega1.m, omega2.m and omega3.m for the dispersion relations.



disp('  ')
disp(' There are three choices of dispersion relation ')
disp(' Enter 1 for the linearized Klein-Gordon equation ')
disp(' Enter 2 for the linearized Korteweg-deVries equation ')
disp(' Enter 3 for the vibrating beam equation  ')
m = input('Enter the choice of dispersion relation   ')   

t = input('Enter the time t at which you wish to view the solution   ')

K = 64;
N = 4096;

delk = K/N;
k = 0:delk:K - delk;

delx = 2*pi/K;
x = -N*pi/K: delx : N*pi/K -delx;


w = bigu(k);


if m == 1

   ww = exp(-i.*omega1(k).*t).*w;

   z = exp(-i*k*t/sqrt(2)).*w;
 

elseif m ==2

    ww = exp(-i.*omega2(k).*t).*w;

    z = exp(-i.*6.*k.*t).*w;

else

   ww = exp(-i.*omega3(k).*t).*w;

   z = exp(-i*4*k*t).*w;



end


   uu = real(ifft(ww));
   
   u = (N*delk/(2*pi))*[uu((N/2)+1:N),uu(1:N/2)];


   uu0 = real(ifft(w));
   u0 = (N*delk/(2*pi))*[uu0((N/2)+1:N), uu0(1:N/2)];

   vv = real(ifft(z));
   v = (N*delk/(2*pi))*[vv((N/2)+1:N),vv(1:N/2)];


  plot(x,u0,x,u,'g')





