

%                        Program trans(mission)
%
%    This program solves the transmission problem of two strings joined
%    at the origin. The speed of propagation on the right is set to 1.
%    User must enter the value "cl" of the speed on the left.
%       The  program solves two problems. In the first case a wave G(x+t)
%    is incident from the right. The function G(x) is provided in the
%    mfile bigg.m.  Use the second data choice in the file bigg.m. The program
%    calculates and plots snapshots of the solution at times t1,t2,t3,
%    and t4. The snapshots are plotted on [-10, 10].
%       The  second problem involves a sinusoidal wave incident from
%    the right of frequency omega and wave number k_right. Because
%    c_right = 1, omega = k_right.  User enters frequency omega and
%    the speed of propagation cl.  The program calculates the wave
%    number k_left, and the amplitude A of the transmitted wave.
%    The spatial factor of the solution is plotted on [-10, 10].



cl = input(' enter the value of cleft   ')

m = input(' Enter 1 for problem 1,  2 for problem 2  ')

if m == 1 

   refl = (1-cl)/(1+cl);
   tr = 2*cl/(1+cl);
 
   fprintf(' Reflection Coefficient is %g.\n', refl )
   fprintf(' Transmission Coefficient is %g\n', tr )

   t = input('Enter the times in the form [t1,t2,t3,t4]      ')

   t1 = t(1); t2 = t(2);  t3 = t(3); t4 = t(4);

   x = -10:.1:10;



%normalizing factor so that incident wave has height 1.

   r = 1/.7607;

   snap0 = r*bigg(x);

   u = tr*bigg((x+cl*t1)/cl)*r;
   uleft = u(1:100);
   v = r*bigg(x+t1) - refl*r*bigg(t1-x);
   vright = v(101:201);
   snap1 = [uleft, vright];

   u = tr*bigg((x+cl*t2)/cl)*r;
   uleft = u(1:100);
   v = r*bigg(x+t2) -refl*r*bigg(t2-x);
   vright = v(101:201);
   snap2 = [uleft, vright];

   u = tr*bigg((x+cl*t3)/cl)*r;
   uleft = u(1:100);
   v = r*bigg(x+t3) -refl*r*bigg(t3-x);
   vright = v(101:201);
   snap3 = [uleft, vright];

   u = tr*bigg((x+cl*t4)/cl)*r;
   uleft = u(1:100);
   v = r*bigg(x+t4) -refl*r*bigg(t4-x);
   vright = v(101:201);
   snap4 = [uleft, vright];

   plot(x,snap0,x,snap1,x,snap2,x,snap3,x,snap4)

else

   omega = input('Enter the frequency omega    ')

   kl = omega/cl;
   kr = omega;

   v = (x< 0)*cl.*sin(kl*x) + (x > 0).*sin(kr*x);
   x = -10: .1 : 10;
   plot(x, v)
end



