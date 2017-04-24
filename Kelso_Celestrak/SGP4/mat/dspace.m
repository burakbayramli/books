% -----------------------------------------------------------------------------
%
%                            procedure dspace
%
%   this procedure provides deep space contributions to mean elements for
%     perturbing third body.  these effects have been averaged over one
%     revolution of the sun and moon.  for earth resonance effects, the
%     effects have been averaged over no revolutions of the satellite.
%     (mean motion)
%
% Author: 
%   Jeff Beck 
%   beckja@alumni.lehigh.edu
%   1.0 (aug 6, 2006) - update for paper dav
% original comments from Vallado C++ version:
%   author        : david vallado                  719-573-2600   28 jun 2005
%
%   inputs        :
%     d2201, d2211, d3210, d3222, d4410, d4422, d5220, d5232, d5421, d5433       -
%     dedt        -
%     del1, del2, del3  -
%     didt        -
%     dmdt        -
%     dnodt       -
%     domdt       -
%     irez        - flag for resonance           0-none, 1-one day, 2-half day
%     argpo       - argument of perigee
%     argpdot     - argument of perigee dot (rate)
%     t           - time
%     tc          -
%     gsto        - gst
%     xfact       -
%     xlamo       -
%     no          - mean motion
%     atime       -
%     em          - eccentricity
%     ft          -
%     argpm       - argument of perigee
%     inclm       - inclination
%     xli         -
%     mm          - mean anomaly
%     xni         - mean motion
%     nodem      - right ascension of ascending node
%
%   outputs       :
%     atime       -
%     em          - eccentricity
%     argpm       - argument of perigee
%     inclm       - inclination
%     xli         -
%     mm          - mean anomaly
%     xni         -
%     nodem      - right ascension of ascending node
%     dndt        -
%     nm          - mean motion
%
%   locals        :
%     delt        -
%     ft          -
%     theta       -
%     x2li        -
%     x2omi       -
%     xl          -
%     xldot       -
%     xnddt       -
%     xndt        -
%     xomi        -
%
%   coupling      :
%     none        -
%
%   references    :
%     hoots, roehrich, norad spacetrack report #3 1980
%     hoots, norad spacetrack report #6 1986
%     hoots, schumacher and glover 2004
%     vallado, crawford, hujsak, kelso  2006
%  ----------------------------------------------------------------------------*/

function [  atime,  em,     argpm,  inclm,  xli,    mm,     xni,...
            nodem, dndt,   nm]...
         = dspace(...
            d2201,  d2211,  d3210,  d3222,  d4410,  d4422,  d5220,...
            d5232,  d5421,  d5433,  dedt,   del1,   del2,   del3,...
            didt,   dmdt,   dnodt,  domdt,  irez,   argpo,  argpdot,...
            t,      tc,     gsto,   xfact,  xlamo,  no,     atime,...
            em,     argpm,  inclm,  xli,    mm,     xni,    nodem,...
            nm)

   twopi = 2.0 * pi;

   fasx2 = 0.13130908;
   fasx4 = 2.8843198;
   fasx6 = 0.37448087;
   g22   = 5.7686396;
   g32   = 0.95240898;
   g44   = 1.8014998;
   g52   = 1.0508330;
   g54   = 4.4108898;
   rptim = 4.37526908801129966e-3;
   stepp =    720.0;
   stepn =   -720.0;
   step2 = 259200.0;

   % /* ----------- calculate deep space resonance effects ----------- */
   dndt   = 0.0;
   theta  = rem(gsto + tc * rptim, twopi);
   em     = em + dedt * t;

   inclm  = inclm + didt * t;
   argpm  = argpm + domdt * t;
   nodem  = nodem + dnodt * t;
   mm     = mm + dmdt * t;

   % //   sgp4fix for negative inclinations
   % //   the following if statement should be commented out
   % //  if (inclm < 0.0)
   % // {
   % //    inclm  = -inclm;
   % //    argpm  = argpm - pi;
   % //    nodem = nodem + pi;
   % //  }

   % /* - update resonances : numerical (euler-maclaurin) integration - */
   % /* ------------------------- epoch restart ----------------------  */

   % //   sgp4fix for propagator problems
   % //   the following integration works for negative time steps and periods
   % //   the specific changes are unknown because the original code was so convoluted

   % // sgp4fix take out atime = 0.0 and fix for faster operation
   ft    = 0.0;

   if (irez ~= 0)
       % sgp4fix streamline check
       if ((atime == 0.0) || (t * atime <= 0.0) || ...
               (abs(t) < abs(atime)) )
           atime  = 0.0;
           xni    = no;
           xli    = xlamo;
       end
       % sgp4fix move check outside loop
       if (t >= 0.0)
           delt = stepp;
       else
           delt = stepn;
       end

       iretn = 381; %// added for do loop
       iret  =   0; %// added for loop
       while (iretn == 381)
           % /* ------------------- dot terms calculated ------------- */
           % /* ----------- near - synchronous resonance terms ------- */
           if (irez ~= 2)
               xndt  = del1 * sin(xli - fasx2) + del2 * sin(2.0 * (xli - fasx4)) +...
                   del3 * sin(3.0 * (xli - fasx6));
               xldot = xni + xfact;
               xnddt = del1 * cos(xli - fasx2) +...
                   2.0 * del2 * cos(2.0 * (xli - fasx4)) +...
                   3.0 * del3 * cos(3.0 * (xli - fasx6));
               xnddt = xnddt * xldot;
           else
               % /* --------- near - half-day resonance terms -------- */
               xomi  = argpo + argpdot * atime;
               x2omi = xomi + xomi;
               x2li  = xli + xli;
               xndt  = d2201 * sin(x2omi + xli - g22) + d2211 * sin(xli - g22) +...
                   d3210 * sin(xomi + xli - g32)  + d3222 * sin(-xomi + xli - g32)+...
                   d4410 * sin(x2omi + x2li - g44)+ d4422 * sin(x2li - g44) +...
                   d5220 * sin(xomi + xli - g52)  + d5232 * sin(-xomi + xli - g52)+...
                   d5421 * sin(xomi + x2li - g54) + d5433 * sin(-xomi + x2li - g54);
               xldot = xni + xfact;
               xnddt = d2201 * cos(x2omi + xli - g22) + d2211 * cos(xli - g22) +...
                   d3210 * cos(xomi + xli - g32) + d3222 * cos(-xomi + xli - g32) +...
                   d5220 * cos(xomi + xli - g52) + d5232 * cos(-xomi + xli - g52) +...
                   2.0 * (d4410 * cos(x2omi + x2li - g44) +...
                   d4422 * cos(x2li - g44) + d5421 * cos(xomi + x2li - g54) +...
                   d5433 * cos(-xomi + x2li - g54));
               xnddt = xnddt * xldot;
           end

           % /* ----------------------- integrator ------------------- */
           % sgp4fix move end checks to end of routine
           if (abs(t - atime) >= stepp)
                iret  = 0;
                iretn = 381;
            else
                ft    = t - atime;
                iretn = 0;
            end

           if (iretn == 381)
               xli   = xli + xldot * delt + xndt * step2;
               xni   = xni + xndt * delt + xnddt * step2;
               atime = atime + delt;
           end
       end %  // while iretn = 381

       nm = xni + xndt * ft + xnddt * ft * ft * 0.5;
       xl = xli + xldot * ft + xndt * ft * ft * 0.5;
       if (irez ~= 1)
           mm   = xl - 2.0 * nodem + 2.0 * theta;
           dndt = nm - no;
       else
           mm   = xl - nodem - argpm+ theta;
           dndt = nm - no;
       end

       nm = no + dndt;
   end

   global idebug dbgfile
   if idebug
       debug4;
   end

   return;
