% -----------------------------------------------------------------------------
%
%                            procedure dsinit
%
%   this procedure provides deep space contributions to mean motion dot due
%     to geopotential resonance with half day and one day orbits.
%
% Author: 
%   Jeff Beck 
%   beckja@alumni.lehigh.edu
%   1.0 (aug 7, 2006) - update for paper dav
% original comments from Vallado C++ version:
%   author        : david vallado                  719-573-2600   28 jun 2005
%
%   inputs        :
%     Cosim, Sinim-
%     Emsq        - Eccentricity squared
%     Argpo       - Argument of Perigee
%     S1, S2, S3, S4, S5      -
%     Ss1, Ss2, Ss3, Ss4, Ss5 -
%     Sz1, Sz3, Sz11, Sz13, Sz21, Sz23, Sz31, Sz33 -
%     T           - Time
%     Tc          -
%     GSTo        - Greenwich sidereal time                   rad
%     Mo          - Mean Anomaly
%     MDot        - Mean Anomaly dot (rate)
%     No          - Mean Motion
%     nodeo       - right ascension of ascending node
%     nodeDot     - right ascension of ascending node dot (rate)
%     XPIDOT      -
%     Z1, Z3, Z11, Z13, Z21, Z23, Z31, Z33 -
%     Eccm        - Eccentricity
%     Argpm       - Argument of perigee
%     Inclm       - Inclination
%     Mm          - Mean Anomaly
%     Xn          - Mean Motion
%     nodem       - right ascension of ascending node
%
%   outputs       :
%     em          - eccentricity
%     argpm       - argument of perigee
%     inclm       - inclination
%     mm          - mean anomaly
%     nm          - mean motion
%     nodem       - right ascension of ascending node
%     irez        - flag for resonance           0-none, 1-one day, 2-half day
%     atime       -
%     d2201, d2211, d3210, d3222, d4410, d4422, d5220, d5232, d5421, d5433    -
%     dedt        -
%     didt        -
%     dmdt        -
%     dndt        -
%     dnodt       -
%     domdt       -
%     del1, del2, del3        -
%     Ses  , Sghl , Sghs , Sgs  , Shl  , Shs  , Sis  , Sls
%     theta       -
%     xfact       -
%     xlamo       -
%     xli         -
%     xni
%
%   locals        :
%     ainv2       -
%     aonv        -
%     cosisq      -
%     eoc         -
%     f220, f221, f311, f321, f322, f330, f441, f442, f522, f523, f542, f543  -
%     g200, g201, g211, g300, g310, g322, g410, g422, g520, g521, g532, g533  -
%     sini2       -
%     temp        -
%     temp1       -
%     theta       -
%     xno2        -
%
%   coupling      :
%     getgravconst
%
%   references    :
%     hoots, roehrich, norad spacetrack report #3 1980
%     hoots, norad spacetrack report #6 1986
%     hoots, schumacher and glover 2004
%     vallado, crawford, hujsak, kelso  2006
%  ----------------------------------------------------------------------------*/

function [  em,     argpm,  inclm,  mm,     nm,     nodem, irez,...
            atime,  d2201,  d2211,  d3210,  d3222,  d4410,  d4422,...
            d5220,  d5232,  d5421,  d5433,  dedt,   didt,   dmdt,...
            dndt,   dnodt,  domdt,  del1,   del2,   del3,   xfact,...
            xlamo,  xli,    xni]...
          = dsinit( ...
            cosim,  emsq,   argpo,  s1,     s2,     s3,     s4,...
            s5,     sinim,  ss1,    ss2,    ss3,    ss4,    ss5,...
            sz1,    sz3,    sz11,   sz13,   sz21,   sz23,   sz31,...
            sz33,   t,      tc,     gsto,   mo,     mdot,   no,...
            nodeo, nodedot,       xpidot, z1,     z3,     z11,...
            z13,    z21,    z23,    z31,    z33,    em,     argpm,...
            inclm,  mm,     nm,     nodem, ecco,   eccsq)

   % /* --------------------- local variables ------------------------ */
   twopi = 2.0 * pi;
   aonv  = 0.0;
   q22    = 1.7891679e-6;
   q31    = 2.1460748e-6;
   q33    = 2.2123015e-7;
   root22 = 1.7891679e-6;
   root44 = 7.3636953e-9;
   root54 = 2.1765803e-9;
   rptim  = 4.37526908801129966e-3;
   root32 = 3.7393792e-7;
   root52 = 1.1428639e-7;
   x2o3   = 2.0 / 3.0;
   znl    = 1.5835218e-4;
   zns    = 1.19459e-5;

   %     // sgp4fix identify constants and allow alternate values
   global tumin mu radiusearthkm xke j2 j3 j4 j3oj2  

   % /* -------------------- deep space initialization ------------ */
   irez = 0;
   if ((nm < 0.0052359877) && (nm > 0.0034906585))
       irez = 1;
   end
   if ((nm >= 8.26e-3) && (nm <= 9.24e-3) && (em >= 0.5))
       irez = 2;
   end
   d2201 = 0;
   d2211 = 0;
   d3210 = 0;
   d3222 = 0;
   d4410 = 0;
   d4422 = 0;
   d5220 = 0;
   d5232 = 0;
   d5421 = 0;
   d5433 = 0;
   del1  = 0;
   del2  = 0;
   del3  = 0;
   atime = 0;
   xfact = 0;
   xlamo = 0;
   xli   = 0;
   xni   = 0;

   % /* ------------------------ do solar terms ------------------- */
   ses  =  ss1 * zns * ss5;
   sis  =  ss2 * zns * (sz11 + sz13);
   sls  = -zns * ss3 * (sz1 + sz3 - 14.0 - 6.0 * emsq);
   sghs =  ss4 * zns * (sz31 + sz33 - 6.0);
   shs  = -zns * ss2 * (sz21 + sz23);
   %   // sgp4fix for 180 deg incl
   if ((inclm < 5.2359877e-2) | (inclm > pi - 5.2359877e-2))
       shs = 0.0;
   end
   if (sinim ~= 0.0)
       shs = shs / sinim;
   end
   sgs  = sghs - cosim * shs;

   % /* ------------------------- do lunar terms ------------------ */
   dedt = ses + s1 * znl * s5;
   didt = sis + s2 * znl * (z11 + z13);
   dmdt = sls - znl * s3 * (z1 + z3 - 14.0 - 6.0 * emsq);
   sghl = s4 * znl * (z31 + z33 - 6.0);
   shll = -znl * s2 * (z21 + z23);
   %   // sgp4fix for 180 deg incl
   if ((inclm < 5.2359877e-2) | (inclm > pi - 5.2359877e-2))
       shll = 0.0;
   end
   domdt = sgs + sghl;
   dnodt = shs;
   if (sinim ~= 0.0)
       domdt = domdt - cosim / sinim * shll;
       dnodt = dnodt + shll / sinim;
   end

   % /* ----------- calculate deep space resonance effects -------- */
   dndt   = 0.0;
   theta  = rem(gsto + tc * rptim, twopi);
   em     = em + dedt * t;
   inclm  = inclm + didt * t;
   argpm  = argpm + domdt * t;
   nodem  = nodem + dnodt * t;
   mm     = mm + dmdt * t;
   % //   sgp4fix for negative inclinations
   % //   the following if statement should be commented out
   % //if (inclm < 0.0)
   % //  {
   % //    inclm  = -inclm;
   % //    argpm  = argpm - pi;
   % //    nodem = nodem + pi;
   % //  }

   %  /* - update resonances : numerical (euler-maclaurin) integration - */
   %  /* ------------------------- epoch restart ----------------------  */
   %  //   sgp4fix for propagator problems
   %  //   the following integration works for negative time steps and periods
   %  //   the specific changes are unknown because the original code was so convoluted

   % /* -------------- initialize the resonance terms ------------- */
   if (irez ~= 0)
       aonv = (nm / xke)^x2o3;

       % /* ---------- geopotential resonance for 12 hour orbits ------ */
       if (irez == 2)
           cosisq = cosim * cosim;
           emo    = em;
           em     = ecco;
           emsqo  = emsq;
           emsq   = eccsq;
           eoc    = em * emsq;
           g201   = -0.306 - (em - 0.64) * 0.440;

           if (em <= 0.65)
               g211 =    3.616  -  13.2470 * em +  16.2900 * emsq;
               g310 =  -19.302  + 117.3900 * em - 228.4190 * emsq +  156.5910 * eoc;
               g322 =  -18.9068 + 109.7927 * em - 214.6334 * emsq +  146.5816 * eoc;
               g410 =  -41.122  + 242.6940 * em - 471.0940 * emsq +  313.9530 * eoc;
               g422 = -146.407  + 841.8800 * em - 1629.014 * emsq + 1083.4350 * eoc;
               g520 = -532.114  + 3017.977 * em - 5740.032 * emsq + 3708.2760 * eoc;
           else
               g211 =   -72.099 +   331.819 * em -   508.738 * emsq +   266.724 * eoc;
               g310 =  -346.844 +  1582.851 * em -  2415.925 * emsq +  1246.113 * eoc;
               g322 =  -342.585 +  1554.908 * em -  2366.899 * emsq +  1215.972 * eoc;
               g410 = -1052.797 +  4758.686 * em -  7193.992 * emsq +  3651.957 * eoc;
               g422 = -3581.690 + 16178.110 * em - 24462.770 * emsq + 12422.520 * eoc;
               if (em > 0.715)
                   g520 =-5149.66 + 29936.92 * em - 54087.36 * emsq + 31324.56 * eoc;
               else
                   g520 = 1464.74 -  4664.75 * em +  3763.64 * emsq;
               end
           end
           if (em < 0.7)
               g533 = -919.22770 + 4988.6100 * em - 9064.7700 * emsq + 5542.21  * eoc;
               g521 = -822.71072 + 4568.6173 * em - 8491.4146 * emsq + 5337.524 * eoc;
               g532 = -853.66600 + 4690.2500 * em - 8624.7700 * emsq + 5341.4  * eoc;
           else
               g533 =-37995.780 + 161616.52 * em - 229838.20 * emsq + 109377.94 * eoc;
               g521 =-51752.104 + 218913.95 * em - 309468.16 * emsq + 146349.42 * eoc;
               g532 =-40023.880 + 170470.89 * em - 242699.48 * emsq + 115605.82 * eoc;
           end

           sini2=  sinim * sinim;
           f220 =  0.75 * (1.0 + 2.0 * cosim+cosisq);
           f221 =  1.5 * sini2;
           f321 =  1.875 * sinim  *  (1.0 - 2.0 * cosim - 3.0 * cosisq);
           f322 = -1.875 * sinim  *  (1.0 + 2.0 * cosim - 3.0 * cosisq);
           f441 = 35.0 * sini2 * f220;
           f442 = 39.3750 * sini2 * sini2;
           f522 =  9.84375 * sinim * (sini2 * (1.0 - 2.0 * cosim- 5.0 * cosisq) +...
               0.33333333 * (-2.0 + 4.0 * cosim + 6.0 * cosisq) );
           f523 = sinim * (4.92187512 * sini2 * (-2.0 - 4.0 * cosim +...
               10.0 * cosisq) + 6.56250012 * (1.0+2.0 * cosim - 3.0 * cosisq));
           f542 = 29.53125 * sinim * (2.0 - 8.0 * cosim+cosisq *...
               (-12.0 + 8.0 * cosim + 10.0 * cosisq));
           f543 = 29.53125 * sinim * (-2.0 - 8.0 * cosim+cosisq *...
               (12.0 + 8.0 * cosim - 10.0 * cosisq));
           xno2  =  nm * nm;
           ainv2 =  aonv * aonv;
           temp1 =  3.0 * xno2 * ainv2;
           temp  =  temp1 * root22;
           d2201 =  temp * f220 * g201;
           d2211 =  temp * f221 * g211;
           temp1 =  temp1 * aonv;
           temp  =  temp1 * root32;
           d3210 =  temp * f321 * g310;
           d3222 =  temp * f322 * g322;
           temp1 =  temp1 * aonv;
           temp  =  2.0 * temp1 * root44;
           d4410 =  temp * f441 * g410;
           d4422 =  temp * f442 * g422;
           temp1 =  temp1 * aonv;
           temp  =  temp1 * root52;
           d5220 =  temp * f522 * g520;
           d5232 =  temp * f523 * g532;
           temp  =  2.0 * temp1 * root54;
           d5421 =  temp * f542 * g521;
           d5433 =  temp * f543 * g533;
           xlamo =  rem(mo + nodeo + nodeo-theta - theta, twopi);
           xfact =  mdot + dmdt + 2.0 * (nodedot + dnodt - rptim) - no;
           em    = emo;
           emsq  = emsqo;
       end

       % /* ---------------- synchronous resonance terms -------------- */
       if (irez == 1)
           g200  = 1.0 + emsq * (-2.5 + 0.8125 * emsq);
           g310  = 1.0 + 2.0 * emsq;
           g300  = 1.0 + emsq * (-6.0 + 6.60937 * emsq);
           f220  = 0.75 * (1.0 + cosim) * (1.0 + cosim);
           f311  = 0.9375 * sinim * sinim * (1.0 + 3.0 * cosim) - 0.75 * (1.0 + cosim);
           f330  = 1.0 + cosim;
           f330  = 1.875 * f330 * f330 * f330;
           del1  = 3.0 * nm * nm * aonv * aonv;
           del2  = 2.0 * del1 * f220 * g200 * q22;
           del3  = 3.0 * del1 * f330 * g300 * q33 * aonv;
           del1  = del1 * f311 * g310 * q31 * aonv;
           xlamo = rem(mo + nodeo + argpo - theta, twopi);
           xfact = mdot + xpidot - rptim + dmdt + domdt + dnodt - no;
       end

       % /* ------------ for sgp4, initialize the integrator ---------- */
       xli   = xlamo;
       xni   = no;
       atime = 0.0;
       nm    = no + dndt;
   end

   global idebug dbgfile
   if idebug
       debug3;
   end

   return;
