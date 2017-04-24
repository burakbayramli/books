% -----------------------------------------------------------------------------
%
%                              procedure sgp4init
%
%   this procedure initializes variables for sgp4.
%
% Author: 
%   Jeff Beck 
%   beckja@alumni.lehigh.edu
%   1.0 (aug 7, 2006) - update for paper dav
% original comments from Vallado C++ version:
%   author        : david vallado                  719-573-2600   28 jun 2005
%
%   inputs        :
%     satn        - satellite number
%     bstar       - sgp4 type drag coefficient              kg/m2er
%     ecco        - eccentricity
%     epoch       - epoch time in days from jan 0, 1950. 0 hr
%     argpo       - argument of perigee (output if ds)
%     inclo       - inclination
%     mo          - mean anomaly (output if ds)
%     no          - mean motion
%     nodeo      - right ascension of ascending node
%
%   outputs       :
%     satrec      - common values for subsequent calls
%     return code - non-zero on error.
%                    1 - mean elements, ecc >= 1.0 or ecc < -0.001 or a < 0.95 er
%                    2 - mean motion less than 0.0
%                    3 - pert elements, ecc < 0.0  or  ecc > 1.0
%                    4 - semi-latus rectum < 0.0
%                    5 - epoch elements are sub-orbital
%                    6 - satellite has decayed
%
%   locals        :
%     CNODM  , SNODM  , COSIM  , SINIM  , COSOMM , SINOMM
%     Cc1sq  , Cc2    , Cc3
%     Coef   , Coef1
%     cosio4      -
%     day         -
%     dndt        -
%     em          - eccentricity
%     emsq        - eccentricity squared
%     eeta        -
%     etasq       -
%     gam         -
%     argpm       - argument of perigee
%     ndem        -
%     inclm       - inclination
%     mm          - mean anomaly
%     nm          - mean motion
%     perige      - perigee
%     pinvsq      -
%     psisq       -
%     qzms24      -
%     rtemsq      -
%     s1, s2, s3, s4, s5, s6, s7          -
%     sfour       -
%     ss1, ss2, ss3, ss4, ss5, ss6, ss7         -
%     sz1, sz2, sz3
%     sz11, sz12, sz13, sz21, sz22, sz23, sz31, sz32, sz33        -
%     tc          -
%     temp        -
%     temp1, temp2, temp3       -
%     tsi         -
%     xpidot      -
%     xhdot1      -
%     z1, z2, z3          -
%     z11, z12, z13, z21, z22, z23, z31, z32, z33         -
%
%   coupling      :
%     getgravconst
%     initl       -
%     dscom       -
%     dpper       -
%     dsinit      -
%     sgp4        -
%
%   references    :
%     hoots, roehrich, norad spacetrack report #3 1980
%     hoots, norad spacetrack report #6 1986
%     hoots, schumacher and glover 2004
%     vallado, crawford, hujsak, kelso  2006
%  ----------------------------------------------------------------------------*/

function [satrec] = sgp4init(whichconst, satrec, xbstar, xecco, epoch, ...
         xargpo, xinclo, xmo, xno, xnodeo);

   % /* ------------------------ initialization --------------------- */
   % /* ----------- set all near earth variables to zero ------------ */
   satrec.isimp   = 0;   satrec.method = 'n'; satrec.aycof    = 0.0;
   satrec.con41   = 0.0; satrec.cc1    = 0.0; satrec.cc4      = 0.0;
   satrec.cc5     = 0.0; satrec.d2     = 0.0; satrec.d3       = 0.0;
   satrec.d4      = 0.0; satrec.delmo  = 0.0; satrec.eta      = 0.0;
   satrec.argpdot = 0.0; satrec.omgcof = 0.0; satrec.sinmao   = 0.0;
   satrec.t       = 0.0; satrec.t2cof  = 0.0; satrec.t3cof    = 0.0;
   satrec.t4cof   = 0.0; satrec.t5cof  = 0.0; satrec.x1mth2   = 0.0;
   satrec.x7thm1  = 0.0; satrec.mdot   = 0.0; satrec.nodedot = 0.0;
   satrec.xlcof   = 0.0; satrec.xmcof  = 0.0; satrec.nodecf  = 0.0;

   % /* ----------- set all deep space variables to zero ------------ */
   satrec.irez  = 0;   satrec.d2201 = 0.0; satrec.d2211 = 0.0;
   satrec.d3210 = 0.0; satrec.d3222 = 0.0; satrec.d4410 = 0.0;
   satrec.d4422 = 0.0; satrec.d5220 = 0.0; satrec.d5232 = 0.0;
   satrec.d5421 = 0.0; satrec.d5433 = 0.0; satrec.dedt  = 0.0;
   satrec.del1  = 0.0; satrec.del2  = 0.0; satrec.del3  = 0.0;
   satrec.didt  = 0.0; satrec.dmdt  = 0.0; satrec.dnodt = 0.0;
   satrec.domdt = 0.0; satrec.e3    = 0.0; satrec.ee2   = 0.0;
   satrec.peo   = 0.0; satrec.pgho  = 0.0; satrec.pho   = 0.0;
   satrec.pinco = 0.0; satrec.plo   = 0.0; satrec.se2   = 0.0;
   satrec.se3   = 0.0; satrec.sgh2  = 0.0; satrec.sgh3  = 0.0;
   satrec.sgh4  = 0.0; satrec.sh2   = 0.0; satrec.sh3   = 0.0;
   satrec.si2   = 0.0; satrec.si3   = 0.0; satrec.sl2   = 0.0;
   satrec.sl3   = 0.0; satrec.sl4   = 0.0; satrec.gsto  = 0.0;
   satrec.xfact = 0.0; satrec.xgh2  = 0.0; satrec.xgh3  = 0.0;
   satrec.xgh4  = 0.0; satrec.xh2   = 0.0; satrec.xh3   = 0.0;
   satrec.xi2   = 0.0; satrec.xi3   = 0.0; satrec.xl2   = 0.0;
   satrec.xl3   = 0.0; satrec.xl4   = 0.0; satrec.xlamo = 0.0;
   satrec.zmol  = 0.0; satrec.zmos  = 0.0; satrec.atime = 0.0;
   satrec.xli   = 0.0; satrec.xni   = 0.0;

   % sgp4fix - note the following variables are also passed directly via satrec. 
   % it is possible to streamline the sgp4init call by deleting the "x"
   % variables, but the user would need to set the satrec.* values first. we
   % include the additional assignment in case twoline2rv is not used. 
   satrec.bstar      = xbstar;
   satrec.ecco       = xecco;
   satrec.argpo      = xargpo;
   satrec.inclo      = xinclo;
   satrec.mo         = xmo;
   satrec.no         = xno;
   satrec.nodeo      = xnodeo;

   %     /* -------------------- wgs-72 earth constants ----------------- */
   %     // sgp4fix identify constants and allow alternate values
   global tumin mu radiusearthkm xke j2 j3 j4 j3oj2  
   [tumin, mu, radiusearthkm, xke, j2, j3, j4, j3oj2] = getgravc( whichconst );

   ss     = 78.0 / radiusearthkm + 1.0;
   qzms2t = ((120.0 - 78.0) / radiusearthkm)^4;
   x2o3   =  2.0 / 3.0;
   % sgp4fix divisor for divide by zero check on inclination
   % the old check used 1.0 + cos(pi-1.0e-9), but then compared it to
   % 1.5 e-12, so the threshold was changed to 1.5e-12 for consistancy
   temp4    =   1.5e-12;

   satrec.init = 'y';
   satrec.t    = 0.0;

   [ainv,  ao,     satrec.con41,   con42,  cosio,  cosio2, einv,   eccsq,...
           satrec.method,  omeosq, posq,   rp,     rteosq, sinio,...
           satrec.gsto,    satrec.no]...
           = initl(...
           satrec.ecco,    epoch,  satrec.inclo,   satrec.no,...
           satrec.satnum);

   satrec.error = 0;
       
   % sgp4fix remove this check as it is unnecessary
   % the mrt check in sgp4 handles decaying satellite cases even if the starting
   %if (rp < 1.0)
   %   printf("# *** satn%d epoch elts sub-orbital ***\n", satn);
   %    satrec.error = 5;
   %end

   if ((omeosq >= 0.0 ) | ( satrec.no >= 0.0))
       satrec.isimp = 0;
       if (rp < (220.0 / radiusearthkm + 1.0))
           satrec.isimp = 1;
       end
       sfour  = ss;
       qzms24 = qzms2t;
       perige = (rp - 1.0) * radiusearthkm;

       % /* - for perigees below 156 km, s and qoms2t are altered - */
       if (perige < 156.0)
           sfour = perige - 78.0;
           if (perige < 98.0)
               sfour = 20.0;
           end
           qzms24 = ((120.0 - sfour) / radiusearthkm)^4.0;
           sfour  = sfour / radiusearthkm + 1.0;
       end
       pinvsq = 1.0 / posq;

       tsi  = 1.0 / (ao - sfour);
       satrec.eta  = ao * satrec.ecco * tsi;
       etasq = satrec.eta * satrec.eta;
       eeta  = satrec.ecco * satrec.eta;
       psisq = abs(1.0 - etasq);
       coef  = qzms24 * tsi^4.0;
       coef1 = coef / psisq^3.5;
       cc2   = coef1 * satrec.no * (ao * (1.0 + 1.5 * etasq + eeta *...
           (4.0 + etasq)) + 0.375 * j2 * tsi / psisq * satrec.con41 *...
           (8.0 + 3.0 * etasq * (8.0 + etasq)));
       satrec.cc1   = satrec.bstar * cc2;
       cc3   = 0.0;
       if (satrec.ecco > 1.0e-4)
           cc3 = -2.0 * coef * tsi * j3oj2 * satrec.no * sinio / satrec.ecco;
       end
       satrec.x1mth2 = 1.0 - cosio2;
       satrec.cc4    = 2.0* satrec.no * coef1 * ao * omeosq *...
           (satrec.eta * (2.0 + 0.5 * etasq) + satrec.ecco *...
           (0.5 + 2.0 * etasq) - j2 * tsi / (ao * psisq) *...
           (-3.0 * satrec.con41 * (1.0 - 2.0 * eeta + etasq *...
           (1.5 - 0.5 * eeta)) + 0.75 * satrec.x1mth2 *...
           (2.0 * etasq - eeta * (1.0 + etasq)) * cos(2.0 * satrec.argpo)));
       satrec.cc5 = 2.0 * coef1 * ao * omeosq * (1.0 + 2.75 *...
           (etasq + eeta) + eeta * etasq);
       cosio4 = cosio2 * cosio2;
       temp1  = 1.5 * j2 * pinvsq * satrec.no;
       temp2  = 0.5 * temp1 * j2 * pinvsq;
       temp3  = -0.46875 * j4 * pinvsq * pinvsq * satrec.no;
       satrec.mdot     = satrec.no + 0.5 * temp1 * rteosq * satrec.con41 +...
           0.0625 * temp2 * rteosq * (13.0 - 78.0 * cosio2 + 137.0 * cosio4);
       satrec.argpdot  = -0.5 * temp1 * con42 + 0.0625 * temp2 *...
           (7.0 - 114.0 * cosio2 + 395.0 * cosio4) +...
           temp3 * (3.0 - 36.0 * cosio2 + 49.0 * cosio4);
       xhdot1            = -temp1 * cosio;
       satrec.nodedot = xhdot1 + (0.5 * temp2 * (4.0 - 19.0 * cosio2) +...
           2.0 * temp3 * (3.0 - 7.0 * cosio2)) * cosio;
       xpidot            =  satrec.argpdot+ satrec.nodedot;
       satrec.omgcof   = satrec.bstar * cc3 * cos(satrec.argpo);
       satrec.xmcof    = 0.0;
       if (satrec.ecco > 1.0e-4)
           satrec.xmcof = -x2o3 * coef * satrec.bstar / eeta;
       end
       satrec.nodecf = 3.5 * omeosq * xhdot1 * satrec.cc1;
       satrec.t2cof   = 1.5 * satrec.cc1;

       % // sgp4fix for divide by zero with xinco = 180 deg
       if (abs(cosio+1.0) > 1.5e-12)
          satrec.xlcof   = -0.25 * j3oj2 * sinio *...
              (3.0 + 5.0 * cosio) / (1.0 + cosio);
       else
         satrec.xlcof   = -0.25 * j3oj2 * sinio *...
              (3.0 + 5.0 * cosio) / temp4;
       end   
       satrec.aycof   = -0.5 * j3oj2 * sinio;
       satrec.delmo   = (1.0 + satrec.eta * cos(satrec.mo))^3;
       satrec.sinmao  = sin(satrec.mo);
       satrec.x7thm1  = 7.0 * cosio2 - 1.0;

       % /* --------------- deep space initialization ------------- */
       if ((2*pi / satrec.no) >= 225.0)
           satrec.method = 'd';
           satrec.isimp  = 1;
           tc    =  0.0;
           inclm = satrec.inclo;

           [sinim,cosim,sinomm,cosomm,snodm,cnodm,day,satrec.e3,satrec.ee2,...
               em,emsq,gam,satrec.peo,satrec.pgho,satrec.pho,satrec.pinco,...
               satrec.plo,rtemsq,satrec.se2,satrec.se3,satrec.sgh2,...
               satrec.sgh3,satrec.sgh4,satrec.sh2,satrec.sh3,satrec.si2,...
               satrec.si3,satrec.sl2,satrec.sl3,satrec.sl4,s1,s2,s3,s4,s5,...
               s6,s7,ss1,ss2,ss3,ss4,ss5,ss6,ss7,sz1,sz2,sz3,sz11,sz12,...
               sz13,sz21,sz22,sz23,sz31,sz32,sz33,satrec.xgh2,satrec.xgh3,...
               satrec.xgh4,satrec.xh2,satrec.xh3,satrec.xi2,satrec.xi3,...
               satrec.xl2,satrec.xl3,satrec.xl4,nm,z1,z2,z3,z11,z12,z13,...
               z21,z22,z23,z31,z32,z33,satrec.zmol,satrec.zmos] = ...
               dscom(epoch,satrec.ecco,satrec.argpo,tc,satrec.inclo,...
               satrec.nodeo,satrec.no);

           [satrec.ecco,satrec.inclo,satrec.nodeo,satrec.argpo,satrec.mo]...
               = dpper(satrec.e3,satrec.ee2,satrec.peo,satrec.pgho,...
               satrec.pho,satrec.pinco,satrec.plo,satrec.se2,satrec.se3,...
               satrec.sgh2,satrec.sgh3,satrec.sgh4,satrec.sh2,satrec.sh3,...
               satrec.si2,satrec.si3,satrec.sl2,satrec.sl3,satrec.sl4,...
               satrec.t,satrec.xgh2,satrec.xgh3,satrec.xgh4,satrec.xh2,...
               satrec.xh3,satrec.xi2,satrec.xi3,satrec.xl2,satrec.xl3,...
               satrec.xl4,satrec.zmol,satrec.zmos,inclm,satrec.init,...
               satrec.ecco,satrec.inclo,satrec.nodeo,satrec.argpo,satrec.mo);

           argpm  = 0.0;
           nodem  = 0.0;
           mm     = 0.0;

           [em,argpm,inclm,mm,nm,nodem,satrec.irez,satrec.atime,...
               satrec.d2201,satrec.d2211,satrec.d3210,satrec.d3222,...
               satrec.d4410,satrec.d4422,satrec.d5220,satrec.d5232,...
               satrec.d5421,satrec.d5433,satrec.dedt,satrec.didt,...
               satrec.dmdt,dndt,satrec.dnodt,satrec.domdt,satrec.del1,...
               satrec.del2,satrec.del3,...
               ... %ses,sghl,sghs,sgs,shl,shs,sis,sls,theta,...
               satrec.xfact,satrec.xlamo,satrec.xli,satrec.xni] ...
               = dsinit(...
               cosim,emsq,satrec.argpo,s1,s2,s3,s4,s5,sinim,ss1,ss2,ss3,...
               ss4,ss5,sz1,sz3,sz11,sz13,sz21,sz23,sz31,sz33,satrec.t,tc,...
               satrec.gsto,satrec.mo,satrec.mdot,satrec.no,satrec.nodeo,...
               satrec.nodedot,xpidot,z1,z3,z11,z13,z21,z23,z31,z33,em,...
               argpm,inclm,mm,nm,nodem,satrec.ecco,eccsq);
       end

       % /* ----------- set variables if not deep space ----------- */
       if (satrec.isimp ~= 1)
           cc1sq          = satrec.cc1 * satrec.cc1;
           satrec.d2    = 4.0 * ao * tsi * cc1sq;
           temp           = satrec.d2 * tsi * satrec.cc1 / 3.0;
           satrec.d3    = (17.0 * ao + sfour) * temp;
           satrec.d4    = 0.5 * temp * ao * tsi *...
               (221.0 * ao + 31.0 * sfour) * satrec.cc1;
           satrec.t3cof = satrec.d2 + 2.0 * cc1sq;
           satrec.t4cof = 0.25 * (3.0 * satrec.d3 + satrec.cc1 *...
               (12.0 * satrec.d2 + 10.0 * cc1sq));
           satrec.t5cof = 0.2 * (3.0 * satrec.d4 +...
               12.0 * satrec.cc1 * satrec.d3 +...
               6.0 * satrec.d2 * satrec.d2 +...
               15.0 * cc1sq * (2.0 * satrec.d2 + cc1sq));
       end
   end % // if omeosq = 0 ...

   % /* finally propogate to zero epoch to initialise all others. */
   % sgp4fix take out check to let satellites process until they are actually below earth surface
   %if(satrec.error == 0)
       [satrec, r, v] = sgp4(satrec, 0.0);
   %end

   satrec.init = 'n';

   global idebug dbgfile
   if idebug
       debug6;
   end

   return;
