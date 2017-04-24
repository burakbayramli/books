Unit SGP4SDP4;
{           Author:  Dr TS Kelso }
{ Original Version:  1991 Oct 30}
{ Current Revision:  1992 Sep 03}
{          Version:  1.50 }
{        Copyright:  1991-1992, All Rights Reserved }
{$N+}

INTERFACE
  Uses Support,
       SGP_Init,
       SGP_Math,SGP_Time;

Procedure SGP(time : double;
       var pos,vel : vector);
Procedure SGP4(tsince : double;
            var iflag : integer;
          var pos,vel : vector);
Procedure SDP4(tsince : double;
            var iflag : integer;
          var pos,vel : vector);

IMPLEMENTATION
  Uses SGP_Intf;

var
{dpinit}
  eqsq,siniq,cosiq,rteqsq,ao,cosq2,sinomo,cosomo,
  bsq,xlldot,omgdt,xnodot,xnodp                   : double;
{dpsec/dpper}
  xll,omgasm,xnodes,_em,xinc,xn,t                 : double;
  qoms2t                                          : double;

Procedure Define_Derived_Constants;
  begin
  xke    := Sqrt(3600*ge/Cube(xkmper));  {Sqrt(ge) ER^3/min^2}
  qoms2t := Sqr(Sqr(qo-s));              {(qo-s)^4 ER^4}
  end; {Define_Derived_Constants}

Procedure SGP4(tsince : double;
            var iflag : integer;
          var pos,vel : vector);
  label
    9,10,90,100,110,130,140;
  const
    a1     : double = 0;  a3ovk2 : double = 0;  ao     : double = 0;
    aodp   : double = 0;  aycof  : double = 0;  betao  : double = 0;
    betao2 : double = 0;  c1     : double = 0;  c1sq   : double = 0;
    c2     : double = 0;  c3     : double = 0;  c4     : double = 0;
    c5     : double = 0;  coef   : double = 0;  coef1  : double = 0;
    cosio  : double = 0;  d2     : double = 0;  d3     : double = 0;
    d4     : double = 0;  del1   : double = 0;  delmo  : double = 0;
    delo   : double = 0;  eeta   : double = 0;  eosq   : double = 0;
    eta    : double = 0;  etasq  : double = 0;  isimp  : integer = 0;
    omgcof : double = 0;  omgdot : double = 0;  perige : double = 0;
    pinvsq : double = 0;  psisq  : double = 0;  qoms24 : double = 0;
    s4     : double = 0;  sinio  : double = 0;  sinmo  : double = 0;
    t2cof  : double = 0;  t3cof  : double = 0;  t4cof  : double = 0;
    t5cof  : double = 0;  temp   : double = 0;  temp1  : double = 0;
    temp2  : double = 0;  temp3  : double = 0;  theta2 : double = 0;
    theta4 : double = 0;  tsi    : double = 0;  x1m5th : double = 0;
    x1mth2 : double = 0;  x3thm1 : double = 0;  x7thm1 : double = 0;
    xhdot1 : double = 0;  xlcof  : double = 0;  xmcof  : double = 0;
    xmdot  : double = 0;  xnodcf : double = 0;  xnodot : double = 0;
    xnodp  : double = 0;
  var
    i                                                  : integer;
    cosuk,sinuk,rfdotk,vx,vy,vz,ux,uy,uz,xmy,xmx,
    cosnok,sinnok,cosik,sinik,rdotk,xinck,xnodek,uk,rk,
    cos2u,sin2u,u,sinu,cosu,betal,rfdot,rdot,r,pl,elsq,
    esine,ecose,epw,temp6,temp5,temp4,cosepw,sinepw,
    capu,ayn,xlt,aynl,xll,axn,xn,beta,xl,e,a,tfour,
    tcube,delm,delomg,templ,tempe,tempa,xnode,tsq,xmp,
    omega,xnoddf,omgadf,xmdf,x,y,z,xdot,ydot,zdot      : double;
  begin
{ Recover original mean motion (xnodp) and semimajor axis (aodp) }
{ from input elements. }
  if (iflag = 0) then
    goto 100;
  a1 := Power(xke/xno,tothrd);
  cosio := Cos(xincl);
  theta2 := cosio*cosio;
  x3thm1 := 3*theta2 - 1;
  eosq := eo*eo;
  betao2 := 1 - eosq;
  betao := sqrt(betao2);
  del1 := 1.5*ck2*x3thm1/(a1*a1*betao*betao2);
  ao := a1*(1 - del1*(0.5*tothrd + del1*(1 + 134/81*del1)));
  delo := 1.5*ck2*x3thm1/(ao*ao*betao*betao2);
  xnodp := xno/(1 + delo);
  aodp := ao/(1 - delo);
{ Initialization }
{ For perigee less than 220 kilometers, the isimp flag is set and
  the equations are truncated to linear variation in sqrt a and
  quadratic variation in mean anomaly.  Also, the c3 term, the
  delta omega term, and the delta m term are dropped. }
  isimp := 0;
  if (aodp*(1 - eo)/ae) < (220/xkmper + ae) then
    isimp := 1;
{ For perigee below 156 km, the values of s and qoms2t are altered. }
  s4 := s;
  qoms24 := qoms2t;
  perige := (aodp*(1 - eo) - ae)*xkmper;
  if (perige >= 156) then
    goto 10;
  s4 := perige - 78;
  if (perige > 98) then
    goto 9;
  s4 := 20;
  9:
  qoms24 := Power((120 - s4)*ae/xkmper,4);
  s4 := s4/xkmper + ae;
 10:
  pinvsq := 1/(aodp*aodp*betao2*betao2);
  tsi := 1/(aodp - s4);
  eta := aodp*eo*tsi;
  etasq := eta*eta;
  eeta := eo*eta;
  psisq := abs(1 - etasq);
  coef := qoms24*Power(tsi,4);
  coef1 := coef/Power(psisq,3.5);
  c2 := coef1*xnodp*(aodp*(1 + 1.5*etasq + eeta*(4 + etasq))
      + 0.75*ck2*tsi/psisq*x3thm1*(8 + 3*etasq*(8 + etasq)));
  c1 := bstar*c2;
  sinio := Sin(xincl);
  a3ovk2 := -xj3/ck2*Power(ae,3);
  c3 := coef*tsi*a3ovk2*xnodp*ae*sinio/eo;
  x1mth2 := 1 - theta2;
  c4 := 2*xnodp*coef1*aodp*betao2*(eta*(2 + 0.5*etasq)
      + eo*(0.5 + 2*etasq) - 2*ck2*tsi/(aodp*psisq)
      *(-3*x3thm1*(1 - 2*eeta + etasq*(1.5 - 0.5*eeta))
      + 0.75*x1mth2*(2*etasq - eeta*(1 + etasq))*Cos(2*omegao)));
  c5 := 2*coef1*aodp*betao2*(1 + 2.75*(etasq + eeta) + eeta*etasq);
  theta4 := theta2*theta2;
  temp1 := 3*ck2*pinvsq*xnodp;
  temp2 := temp1*ck2*pinvsq;
  temp3 := 1.25*ck4*pinvsq*pinvsq*xnodp;
  xmdot := xnodp + 0.5*temp1*betao*x3thm1
         + 0.0625*temp2*betao*(13 - 78*theta2 + 137*theta4);
  x1m5th := 1 - 5*theta2;
  omgdot := -0.5*temp1*x1m5th + 0.0625*temp2*(7 - 114*theta2 +395*theta4)
          + temp3*(3 - 36*theta2 + 49*theta4);
  xhdot1 := -temp1*cosio;
  xnodot := xhdot1 + (0.5*temp2*(4 - 19*theta2)
          + 2*temp3*(3 - 7*theta2))*cosio;
  omgcof := bstar*c3*Cos(omegao);
  xmcof := -tothrd*coef*bstar*ae/eeta;
  xnodcf := 3.5*betao2*xhdot1*c1;
  t2cof := 1.5*c1;
  xlcof := 0.125*a3ovk2*sinio*(3 + 5*cosio)/(1 + cosio);
  aycof := 0.25*a3ovk2*sinio;
  delmo := Power(1 + eta*Cos(xmo),3);
  sinmo := Sin(xmo);
  x7thm1 := 7*theta2 - 1;
  if (isimp = 1) then
    goto 90;
  c1sq := c1*c1;
  d2 := 4*aodp*tsi*c1sq;
  temp := d2*tsi*c1/3;
  d3 := (17*aodp + s4)*temp;
  d4 := 0.5*temp*aodp*tsi*(221*aodp + 31*s4)*c1;
  t3cof := d2 + 2*c1sq;
  t4cof := 0.25*(3*d3 + c1*(12*d2 + 10*c1sq));
  t5cof := 0.2*(3*d4 + 12*c1*d3 + 6*d2*d2 + 15*c1sq*(2*d2 + c1sq));
 90:
  iflag := 0;
{ Update for secular gravity and atmospheric drag. }
100:
  xmdf := xmo + xmdot*tsince;
  omgadf := omegao + omgdot*tsince;
  xnoddf := xnodeo + xnodot*tsince;
  omega := omgadf;
  xmp := xmdf;
  tsq := tsince*tsince;
  xnode := xnoddf + xnodcf*tsq;
  tempa := 1 - c1*tsince;
  tempe := bstar*c4*tsince;
  templ := t2cof*tsq;
  if (isimp = 1) then
    goto 110;
  delomg := omgcof*tsince;
  delm := xmcof*(Power(1 + eta*Cos(xmdf),3) - delmo);
  temp := delomg + delm;
  xmp := xmdf + temp;
  omega := omgadf - temp;
  tcube := tsq*tsince;
  tfour := tsince*tcube;
  tempa := tempa - d2*tsq - d3*tcube - d4*tfour;
  tempe := tempe + bstar*c5*(Sin(xmp) - sinmo);
  templ := templ + t3cof*tcube + tfour*(t4cof + tsince*t5cof);
110:
  a := aodp*Sqr(tempa);
  e := eo - tempe;
  xl := xmp + omega + xnode + xnodp*templ;
  beta := sqrt(1 - e*e);
  xn := xke/Power(a,1.5);
{ Long period periodics }
  axn := e*Cos(omega);
  temp := 1/(a*beta*beta);
  xll := temp*xlcof*axn;
  aynl := temp*aycof;
  xlt := xl + xll;
  ayn := e*Sin(omega) + aynl;
{ Solve Kepler's Equation }
  capu := fmod2p(xlt - xnode);
  temp2 := capu;
  for i := 1 to 10 do
    begin
    sinepw := Sin(temp2);
    cosepw := Cos(temp2);
    temp3 := axn*sinepw;
    temp4 := ayn*cosepw;
    temp5 := axn*cosepw;
    temp6 := ayn*sinepw;
    epw := (capu - temp4 + temp3 - temp2)/(1 - temp5 - temp6) + temp2;
    if (abs(epw - temp2) <= e6a) then
      goto 140;
130:
    temp2 := epw;
    end; {for i}
{ Short period preliminary quantities }
140:
  ecose := temp5 + temp6;
  esine := temp3 - temp4;
  elsq := axn*axn + ayn*ayn;
  temp := 1 - elsq;
  pl := a*temp;
  r := a*(1 - ecose);
  temp1 := 1/r;
  rdot := xke*sqrt(a)*esine*temp1;
  rfdot := xke*sqrt(pl)*temp1;
  temp2 := a*temp1;
  betal := sqrt(temp);
  temp3 := 1/(1 + betal);
  cosu := temp2*(cosepw - axn + ayn*esine*temp3);
  sinu := temp2*(sinepw - ayn - axn*esine*temp3);
  u := actan(sinu,cosu);
  sin2u := 2*sinu*cosu;
  cos2u := 2*cosu*cosu - 1;
  temp := 1/pl;
  temp1 := ck2*temp;
  temp2 := temp1*temp;
{ Update for short periodics }
  rk := r*(1 - 1.5*temp2*betal*x3thm1) + 0.5*temp1*x1mth2*cos2u;
  uk := u - 0.25*temp2*x7thm1*sin2u;
  xnodek := xnode + 1.5*temp2*cosio*sin2u;
  xinck := xincl + 1.5*temp2*cosio*sinio*cos2u;
  rdotk := rdot - xn*temp1*x1mth2*sin2u;
  rfdotk := rfdot + xn*temp1*(x1mth2*cos2u + 1.5*x3thm1);
{ Orientation vectors }
  sinuk := Sin(uk);
  cosuk := Cos(uk);
  sinik := Sin(xinck);
  cosik := Cos(xinck);
  sinnok := Sin(xnodek);
  cosnok := Cos(xnodek);
  xmx := -sinnok*cosik;
  xmy := cosnok*cosik;
  ux := xmx*sinuk + cosnok*cosuk;
  uy := xmy*sinuk + sinnok*cosuk;
  uz := sinik*sinuk;
  vx := xmx*cosuk - cosnok*sinuk;
  vy := xmy*cosuk - sinnok*sinuk;
  vz := sinik*cosuk;
{ Position and velocity }
  x := rk*ux;  pos[1] := x;
  y := rk*uy;  pos[2] := y;
  z := rk*uz;  pos[3] := z;
  xdot := rdotk*ux + rfdotk*vx;  vel[1] := xdot;
  ydot := rdotk*uy + rfdotk*vy;  vel[2] := ydot;
  zdot := rdotk*uz + rfdotk*vz;  vel[3] := zdot;
  end; {Procedure SGP4}

Procedure Deep(ideep : integer);
  const
    zns    =  1.19459E-5;     c1ss   =  2.9864797E-6;   zes    =  0.01675;
    znl    =  1.5835218E-4;   c1l    =  4.7968065E-7;   zel    =  0.05490;
    zcosis =  0.91744867;     zsinis =  0.39785416;     zsings = -0.98088458;
    zcosgs =  0.1945905;      zcoshs =  1;              zsinhs =  0;
    q22    =  1.7891679E-6;   q31    =  2.1460748E-6;   q33    =  2.2123015E-7;
    g22    =  5.7686396;      g32    =  0.95240898;     g44    =  1.8014998;
    g52    =  1.0508330;      g54    =  4.4108898;      root22 =  1.7891679E-6;
    root32 =  3.7393792E-7;   root44 =  7.3636953E-9;   root52 =  1.1428639E-7;
    root54 =  2.1765803E-9;   thdt   =  4.3752691E-3;
  label
    {dpinit}
    5,10,20,30,40,45,50,55,60,65,70,80,
    {dpsec}
    90,100,105,110,120,125,130,140,150,152,154,160,165,170,175,180,
    {dpper}
    205,210,218,220,230;
const { Typed constants to retain values between ENTRY calls }
    iresfl : integer = 0;  isynfl : integer = 0;
    iret   : integer = 0;  iretn  : integer = 0;
    ls     : integer = 0;
    a1     : double = 0;    a2     : double = 0;    a3     : double = 0;
    a4     : double = 0;    a5     : double = 0;    a6     : double = 0;
    a7     : double = 0;    a8     : double = 0;    a9     : double = 0;
    a10    : double = 0;    ainv2  : double = 0;    alfdp  : double = 0;
    aqnv   : double = 0;    atime  : double = 0;    betdp  : double = 0;
    bfact  : double = 0;    c      : double = 0;    cc     : double = 0;
    cosis  : double = 0;    cosok  : double = 0;    cosq   : double = 0;
    ctem   : double = 0;    d2201  : double = 0;    d2211  : double = 0;
    d3210  : double = 0;    d3222  : double = 0;    d4410  : double = 0;
    d4422  : double = 0;    d5220  : double = 0;    d5232  : double = 0;
    d5421  : double = 0;    d5433  : double = 0;    dalf   : double = 0;
    day    : double = 0;    dbet   : double = 0;    del1   : double = 0;
    del2   : double = 0;    del3   : double = 0;    delt   : double = 0;
    dls    : double = 0;    e3     : double = 0;    ee2    : double = 0;
    eoc    : double = 0;    eq     : double = 0;    f2     : double = 0;
    f220   : double = 0;    f221   : double = 0;    f3     : double = 0;
    f311   : double = 0;    f321   : double = 0;    f322   : double = 0;
    f330   : double = 0;    f441   : double = 0;    f442   : double = 0;
    f522   : double = 0;    f523   : double = 0;    f542   : double = 0;
    f543   : double = 0;    fasx2  : double = 0;    fasx4  : double = 0;
    fasx6  : double = 0;    ft     : double = 0;    g200   : double = 0;
    g201   : double = 0;    g211   : double = 0;    g300   : double = 0;
    g310   : double = 0;    g322   : double = 0;    g410   : double = 0;
    g422   : double = 0;    g520   : double = 0;    g521   : double = 0;
    g532   : double = 0;    g533   : double = 0;    gam    : double = 0;
    omegaq : double = 0;    pe     : double = 0;    pgh    : double = 0;
    ph     : double = 0;    pinc   : double = 0;    pl     : double = 0;
    preep  : double = 0;    s1     : double = 0;    s2     : double = 0;
    s3     : double = 0;    s4     : double = 0;    s5     : double = 0;
    s6     : double = 0;    s7     : double = 0;    savtsn : double = 0;
    se     : double = 0;    se2    : double = 0;    se3    : double = 0;
    sel    : double = 0;    ses    : double = 0;    sgh    : double = 0;
    sgh2   : double = 0;    sgh3   : double = 0;    sgh4   : double = 0;
    sghl   : double = 0;    sghs   : double = 0;    sh     : double = 0;
    sh2    : double = 0;    sh3    : double = 0;    sh1    : double = 0;
    shs    : double = 0;    si     : double = 0;    si2    : double = 0;
    si3    : double = 0;    sil    : double = 0;    sini2  : double = 0;
    sinis  : double = 0;    sinok  : double = 0;    sinq   : double = 0;
    sinzf  : double = 0;    sis    : double = 0;    sl     : double = 0;
    sl2    : double = 0;    sl3    : double = 0;    sl4    : double = 0;
    sll    : double = 0;    sls    : double = 0;    sse    : double = 0;
    ssg    : double = 0;    ssh    : double = 0;    ssi    : double = 0;
    ssl    : double = 0;    stem   : double = 0;    step2  : double = 0;
    stepn  : double = 0;    stepp  : double = 0;    temp   : double = 0;
    temp1  : double = 0;    thgr   : double = 0;    x1     : double = 0;
    x2     : double = 0;    x2li   : double = 0;    x2omi  : double = 0;
    x3     : double = 0;    x4     : double = 0;    x5     : double = 0;
    x6     : double = 0;    x7     : double = 0;    x8     : double = 0;
    xfact  : double = 0;    xgh2   : double = 0;    xgh3   : double = 0;
    xgh4   : double = 0;    xh2    : double = 0;    xh3    : double = 0;
    xi2    : double = 0;    xi3    : double = 0;    xl     : double = 0;
    xl2    : double = 0;    xl3    : double = 0;    xl4    : double = 0;
    xlamo  : double = 0;    xldot  : double = 0;    xli    : double = 0;
    xls    : double = 0;    xmao   : double = 0;    xnddt  : double = 0;
    xndot  : double = 0;    xni    : double = 0;    xno2   : double = 0;
    xnodce : double = 0;    xnoi   : double = 0;    xnq    : double = 0;
    xomi   : double = 0;    xpidot : double = 0;    xqncl  : double = 0;
    z1     : double = 0;    z11    : double = 0;    z12    : double = 0;
    z13    : double = 0;    z2     : double = 0;    z21    : double = 0;
    z22    : double = 0;    z23    : double = 0;    z3     : double = 0;
    z31    : double = 0;    z32    : double = 0;    z33    : double = 0;
    zcosg  : double = 0;    zcosgl : double = 0;    zcosh  : double = 0;
    zcoshl : double = 0;    zcosi  : double = 0;    zcosil : double = 0;
    ze     : double = 0;    zf     : double = 0;    zm     : double = 0;
    zmo    : double = 0;    zmol   : double = 0;    zmos   : double = 0;
    zn     : double = 0;    zsing  : double = 0;    zsingl : double = 0;
    zsinh  : double = 0;    zsinhl : double = 0;    zsini  : double = 0;
    zsinil : double = 0;    zx     : double = 0;    zy     : double = 0;
  begin
  case ideep of
    dpinit : begin { Entrance for deep space initialization }
             thgr := Thetag(epoch);
             eq := eo;
             xnq := xnodp;
             aqnv := 1/ao;
             xqncl := xincl;
             xmao := xmo;
             xpidot := omgdt + xnodot;
             sinq := Sin(xnodeo);
             cosq := Cos(xnodeo);
             omegaq := omegao;
    { Initialize lunar solar terms }
          5: day := ds50 + 18261.5;  {Days since 1900 Jan 0.5}
             if (day = preep) then
               Goto 10;
             preep := day;
             xnodce := 4.5236020 - 9.2422029E-4*day;
             stem := Sin(xnodce);
             ctem := Cos(xnodce);
             zcosil := 0.91375164 - 0.03568096*ctem;
             zsinil := Sqrt(1 - zcosil*zcosil);
             zsinhl := 0.089683511*stem/zsinil;
             zcoshl := Sqrt(1 - zsinhl*zsinhl);
             c := 4.7199672 + 0.22997150*day;
             gam := 5.8351514 + 0.0019443680*day;
             zmol := fmod2p(c - gam);
             zx := 0.39785416*stem/zsinil;
             zy := zcoshl*ctem + 0.91744867*zsinhl*stem;
             zx := Actan(zx,zy);
             zx := gam + zx - xnodce;
             zcosgl := Cos(zx);
             zsingl := Sin(zx);
             zmos := 6.2565837 + 0.017201977*day;
             zmos := fmod2p(zmos);
    { Do solar terms }
         10: savtsn := 1E20;
             zcosg := zcosgs;
             zsing := zsings;
             zcosi := zcosis;
             zsini := zsinis;
             zcosh := cosq;
             zsinh := sinq;
             cc := c1ss;
             zn := zns;
             ze := zes;
             zmo := zmos;
             xnoi := 1/xnq;
             ls := 30; {assign 30 to ls}
         20: a1 := zcosg*zcosh + zsing*zcosi*zsinh;
             a3 := -zsing*zcosh + zcosg*zcosi*zsinh;
             a7 := -zcosg*zsinh + zsing*zcosi*zcosh;
             a8 := zsing*zsini;
             a9 := zsing*zsinh + zcosg*zcosi*zcosh;
             a10 := zcosg*zsini;
             a2 := cosiq*a7 +  siniq*a8;
             a4 := cosiq*a9 +  siniq*a10;
             a5 := -siniq*a7 +  cosiq*a8;
             a6 := -siniq*a9 +  cosiq*a10;
             x1 := a1*cosomo + a2*sinomo;
             x2 := a3*cosomo + a4*sinomo;
             x3 := -a1*sinomo + a2*cosomo;
             x4 := -a3*sinomo + a4*cosomo;
             x5 := a5*sinomo;
             x6 := a6*sinomo;
             x7 := a5*cosomo;
             x8 := a6*cosomo;
             z31 := 12*x1*x1 - 3*x3*x3;
             z32 := 24*x1*x2 - 6*x3*x4;
             z33 := 12*x2*x2 - 3*x4*x4;
             z1 := 3*(a1*a1 + a2*a2) + z31*eqsq;
             z2 := 6*(a1*a3 + a2*a4) + z32*eqsq;
             z3 := 3*(a3*a3 + a4*a4) + z33*eqsq;
             z11 := -6*a1*a5 + eqsq*(-24*x1*x7 - 6*x3*x5);
             z12 := -6*(a1*a6 + a3*a5)
                  + eqsq*(-24*(x2*x7 + x1*x8) - 6*(x3*x6 + x4*x5));
             z13 := -6*a3*a6 + eqsq*(-24*x2*x8 - 6*x4*x6);
             z21 := 6*a2*a5 + eqsq*(24*x1*x5 - 6*x3*x7);
             z22 := 6*(a4*a5 + a2*a6)
                  + eqsq*(24*(x2*x5 + x1*x6) - 6*(x4*x7 + x3*x8));
             z23 := 6*a4*a6 + eqsq*(24*x2*x6 - 6*x4*x8);
             z1 := z1 + z1 + bsq*z31;
             z2 := z2 + z2 + bsq*z32;
             z3 := z3 + z3 + bsq*z33;
             s3 := cc*xnoi;
             s2 := -0.5*s3/rteqsq;
             s4 := s3*rteqsq;
             s1 := -15*eq*s4;
             s5 := x1*x3 + x2*x4;
             s6 := x2*x3 + x1*x4;
             s7 := x2*x4 - x1*x3;
             se := s1*zn*s5;
             si := s2*zn*(z11 + z13);
             sl := -zn*s3*(z1 + z3 - 14 - 6*eqsq);
             sgh := s4*zn*(z31 + z33 - 6);
             sh := -zn*s2*(z21 + z23);
             if (xqncl < 5.2359877E-2) then
               sh := 0;
             ee2 := 2*s1*s6;
             e3 := 2*s1*s7;
             xi2 := 2*s2*z12;
             xi3 := 2*s2*(z13 - z11);
             xl2 := -2*s3*z2;
             xl3 := -2*s3*(z3 - z1);
             xl4 := -2*s3*(-21 - 9*eqsq)*ze;
             xgh2 := 2*s4*z32;
             xgh3 := 2*s4*(z33 - z31);
             xgh4 := -18*s4*ze;
             xh2 := -2*s2*z22;
             xh3 := -2*s2*(z23 - z21);
             case ls of
               30 : Goto 30;
               40 : Goto 40;
             else
                    Halt;
             end; {case}
    { Do lunar terms }
         30: sse := se;
             ssi := si;
             ssl := sl;
             ssh := sh/siniq;
             ssg := sgh - cosiq*ssh;
             se2 := ee2;
             si2 := xi2;
             sl2 := xl2;
             sgh2 := xgh2;
             sh2 := xh2;
             se3 := e3;
             si3 := xi3;
             sl3 := xl3;
             sgh3 := xgh3;
             sh3 := xh3;
             sl4 := xl4;
             sgh4 := xgh4;
             zcosg := zcosgl;
             zsing := zsingl;
             zcosi := zcosil;
             zsini := zsinil;
             zcosh := zcoshl*cosq + zsinhl*sinq;
             zsinh := sinq*zcoshl - cosq*zsinhl;
             zn := znl;
             cc := c1l;
             ze := zel;
             zmo := zmol;
             ls := 40; {assign 40 to ls}
             Goto 20;
         40: sse := sse + se;
             ssi := ssi + si;
             ssl := ssl + sl;
             ssg := ssg + sgh - cosiq/siniq*sh;
             ssh := ssh + sh/siniq;
    { Geopotential resonance initialization for 12 hour orbits }
             iresfl := 0;
             isynfl := 0;
             if (xnq < 0.0052359877) and (xnq > 0.0034906585) then
               Goto 70;
             if (xnq < 8.26E-3) or (xnq > 9.24E-3) then
               exit;
             if (eq < 0.5) then
               exit;
             iresfl := 1;
             eoc := eq*eqsq;
             g201 := -0.306 - (eq - 0.64)*0.440;
             if (eq > 0.65) then
               Goto 45;
             g211 := 3.616 - 13.247*eq + 16.290*eqsq;
             g310 := -19.302 + 117.390*eq - 228.419*eqsq + 156.591*eoc;
             g322 := -18.9068 + 109.7927*eq - 214.6334*eqsq + 146.5816*eoc;
             g410 := -41.122 + 242.694*eq - 471.094*eqsq + 313.953*eoc;
             g422 := -146.407 + 841.880*eq - 1629.014*eqsq + 1083.435*eoc;
             g520 := -532.114 + 3017.977*eq - 5740*eqsq + 3708.276*eoc;
             Goto 55;
         45: g211 := -72.099 + 331.819*eq - 508.738*eqsq + 266.724*eoc;
             g310 := -346.844 + 1582.851*eq - 2415.925*eqsq + 1246.113*eoc;
             g322 := -342.585 + 1554.908*eq - 2366.899*eqsq + 1215.972*eoc;
             g410 := -1052.797 + 4758.686*eq - 7193.992*eqsq + 3651.957*eoc;
             g422 := -3581.69 + 16178.11*eq - 24462.77*eqsq + 12422.52*eoc;
             if (eq > 0.715) then
               Goto 50;
             g520 := 1464.74 - 4664.75*eq + 3763.64*eqsq;
             Goto 55;
         50: g520 := -5149.66 + 29936.92*eq - 54087.36*eqsq + 31324.56*eoc;
         55: if (eq >= (0.7)) then
               Goto 60;
             g533 := -919.2277 + 4988.61*eq - 9064.77*eqsq + 5542.21*eoc;
             g521 := -822.71072 + 4568.6173*eq - 8491.4146*eqsq + 5337.524*eoc;
             g532 := -853.666 + 4690.25*eq - 8624.77*eqsq + 5341.4*eoc;
             Goto 65;
         60: g533 := -37995.78 + 161616.52*eq - 229838.2*eqsq + 109377.94*eoc;
             g521 := -51752.104 + 218913.95*eq - 309468.16*eqsq + 146349.42*eoc;
             g532 := -40023.88 + 170470.89*eq - 242699.48*eqsq + 115605.82*eoc;
         65: sini2 := siniq*siniq;
             f220 := 0.75*(1 + 2*cosiq + cosq2);
             f221 := 1.5*sini2;
             f321 := 1.875*siniq*(1 - 2*cosiq - 3*cosq2);
             f322 := -1.875*siniq*(1 + 2*cosiq - 3*cosq2);
             f441 := 35*sini2*f220;
             f442 := 39.3750*sini2*sini2;
             f522 := 9.84375*siniq*(sini2*(1 - 2*cosiq - 5*cosq2)
                   + 0.33333333*(-2 + 4*cosiq + 6*cosq2));
             f523 := siniq*(4.92187512*sini2*(-2 - 4*cosiq + 10*cosq2)
                   + 6.56250012*(1 + 2*cosiq - 3*cosq2));
             f542 := 29.53125*siniq*(2 - 8*cosiq + cosq2*(-12 + 8*cosiq + 10*cosq2));
             f543 := 29.53125*siniq*(-2 - 8*cosiq + cosq2*(12 + 8*cosiq - 10*cosq2));
             xno2 := xnq*xnq;
             ainv2 := aqnv*aqnv;
             temp1 := 3*xno2*ainv2;
             temp := temp1*root22;
             d2201 := temp*f220*g201;
             d2211 := temp*f221*g211;
             temp1 := temp1*aqnv;
             temp := temp1*root32;
             d3210 := temp*f321*g310;
             d3222 := temp*f322*g322;
             temp1 := temp1*aqnv;
             temp := 2*temp1*root44;
             d4410 := temp*f441*g410;
             d4422 := temp*f442*g422;
             temp1 := temp1*aqnv;
             temp := temp1*root52;
             d5220 := temp*f522*g520;
             d5232 := temp*f523*g532;
             temp := 2*temp1*root54;
             d5421 := temp*f542*g521;
             d5433 := temp*f543*g533;
             xlamo := xmao + xnodeo + xnodeo - thgr - thgr;
             bfact := xlldot + xnodot + xnodot - thdt - thdt;
             bfact := bfact + ssl + ssh + ssh;
             Goto 80;
    { Synchronous resonance terms initialization }
         70: iresfl := 1;
             isynfl := 1;
             g200 := 1 + eqsq*(-2.5 + 0.8125*eqsq);
             g310 := 1 + 2*eqsq;
             g300 := 1 + eqsq*(-6 + 6.60937*eqsq);
             f220 := 0.75*(1 + cosiq)*(1 + cosiq);
             f311 := 0.9375*siniq*siniq*(1 + 3*cosiq) - 0.75*(1 + cosiq);
             f330 := 1 + cosiq;
             f330 := 1.875*f330*f330*f330;
             del1 := 3*xnq*xnq*aqnv*aqnv;
             del2 := 2*del1*f220*g200*q22;
             del3 := 3*del1*f330*g300*q33*aqnv;
             del1 := del1*f311*g310*q31*aqnv;
             fasx2 := 0.13130908;
             fasx4 := 2.8843198;
             fasx6 := 0.37448087;
             xlamo := xmao + xnodeo + omegao - thgr;
             bfact := xlldot + xpidot - thdt;
             bfact := bfact + ssl + ssg + ssh;
         80: xfact := bfact - xnq;
    { Initialize integrator }
             xli := xlamo;
             xni := xnq;
             atime := 0;
             stepp := 720;
             stepn := -720;
             step2 := 259200;
             end; {dpinit}
    dpsec  : begin { Entrance for deep space secular effects }
             xll := xll + ssl*t;
             omgasm := omgasm + ssg*t;
             xnodes := xnodes + ssh*t;
             _em := eo + sse*t;
             xinc := xincl + ssi*t;
             if (xinc >= 0) then
               Goto 90;
             xinc := -xinc;
             xnodes := xnodes  +  pi;
             omgasm := omgasm - pi;
         90: if (iresfl = 0) then
               exit;
        100: if (atime = 0) then
               Goto 170;
             if (t >= 0) and (atime < 0) then
               Goto 170;
             if (t < 0) and (atime >= 0) then
               Goto 170;
        105: if (Abs(t) >= Abs(atime)) then
               Goto 120;
             delt := stepp;
             if (t >= 0) then
               delt := stepn;
        110: iret := 100; {assign 100 to iret}
             Goto 160;
        120: delt := stepn;
             if (t > 0) then
               delt := stepp;
        125: if (Abs(t - atime) < stepp) then
               Goto 130;
             iret := 125; {assign 125 to iret}
             Goto 160;
        130: ft := t - atime;
             iretn := 140; {assign 140 to iretn}
             Goto 150;
        140: xn := xni + xndot*ft + xnddt*ft*ft*0.5;
             xl := xli + xldot*ft + xndot*ft*ft*0.5;
             temp := -xnodes + thgr + t*thdt;
             xll := xl - omgasm + temp;
             if (isynfl = 0) then
               xll := xl + temp + temp;
             exit;
    { Dot terms calculated }
        150: if (isynfl = 0) then
               Goto 152;
             xndot := del1*Sin(xli - fasx2) + del2*Sin(2*(xli - fasx4))
                    + del3*Sin(3*(xli - fasx6));
             xnddt := del1*Cos(xli - fasx2)
                    + 2*del2*Cos(2*(xli - fasx4))
                    + 3*del3*Cos(3*(xli - fasx6));
             Goto 154;
        152: xomi := omegaq + omgdt*atime;
             x2omi := xomi + xomi;
             x2li := xli + xli;
             xndot := d2201*Sin(x2omi + xli - g22)
                    + d2211*Sin(xli - g22)
                    + d3210*Sin(xomi + xli - g32)
                    + d3222*Sin(-xomi + xli - g32)
                    + d4410*Sin(x2omi + x2li - g44)
                    + d4422*Sin(x2li - g44)
                    + d5220*Sin(xomi + xli - g52)
                    + d5232*Sin(-xomi + xli - g52)
                    + d5421*Sin(xomi + x2li - g54)
                    + d5433*Sin(-xomi + x2li - g54);
             xnddt := d2201*Cos(x2omi + xli - g22)
                    + d2211*Cos(xli - g22)
                    + d3210*Cos(xomi + xli - g32)
                    + d3222*Cos(-xomi + xli - g32)
                    + d5220*Cos(xomi + xli - g52)
                    + d5232*Cos(-xomi + xli - g52)
                    + 2*(d4410*Cos(x2omi + x2li - g44)
                    + d4422*Cos(x2li - g44)
                    + d5421*Cos(xomi + x2li - g54)
                    + d5433*Cos(-xomi + x2li - g54));
        154: xldot := xni + xfact;
             xnddt := xnddt*xldot;
             case iretn of
               140 : Goto 140;
               165 : Goto 165;
             else
                     Halt;
             end; {case}
    { Integrator }
        160: iretn := 165; {assign 165 to iretn}
             Goto 150;
        165: xli := xli + xldot*delt + xndot*step2;
             xni := xni + xndot*delt + xnddt*step2;
             atime := atime + delt;
             case iret of
               100 : Goto 100;
               125 : Goto 125;
             else
                     Halt;
             end; {case}
    { Epoch restart }
        170: if (t >= 0) then
               Goto 175;
             delt := stepn;
             Goto 180;
        175: delt := stepp;
        180: atime := 0;
             xni := xnq;
             xli := xlamo;
             Goto 125;
             end; {dpsec}
    dpper  : begin { Entrance for lunar-solar periodics }
             sinis := Sin(xinc);
             cosis := Cos(xinc);
             if (Abs(savtsn - t) < 30) then
               Goto 210;
             savtsn := t;
             zm := zmos + zns*t;
        205: zf := zm + 2*zes*Sin(zm);
             sinzf := Sin(zf);
             f2 := 0.5*sinzf*sinzf - 0.25;
             f3 := -0.5*sinzf*Cos(zf);
             ses := se2*f2 + se3*f3;
             sis := si2*f2 + si3*f3;
             sls := sl2*f2 + sl3*f3 + sl4*sinzf;
             sghs := sgh2*f2 + sgh3*f3 + sgh4*sinzf;
             shs := sh2*f2 + sh3*f3;
             zm := zmol + znl*t;
             zf := zm + 2*zel*Sin(zm);
             sinzf := Sin(zf);
             f2 := 0.5*sinzf*sinzf - 0.25;
             f3 := -0.5*sinzf*Cos(zf);
             sel := ee2*f2 + e3*f3;
             sil := xi2*f2 + xi3*f3;
             sll := xl2*f2 + xl3*f3 + xl4*sinzf;
             sghl := xgh2*f2 + xgh3*f3 + xgh4*sinzf;
             sh1 := xh2*f2 + xh3*f3;
             pe := ses + sel;
             pinc := sis + sil;
             pl := sls + sll;
        210: pgh := sghs + sghl;
             ph := shs + sh1;
             xinc := xinc + pinc;
             _em := _em + pe;
             if (xqncl < 0.2) then
               Goto 220;
             Goto 218;
    { Apply periodics directly }
        218: ph := ph/siniq;
             pgh := pgh - cosiq*ph;
             omgasm := omgasm + pgh;
             xnodes := xnodes + ph;
             xll := xll + pl;
             Goto 230;
    { Apply periodics with Lyddane modification }
        220: sinok := Sin(xnodes);
             cosok := Cos(xnodes);
             alfdp := sinis*sinok;
             betdp := sinis*cosok;
             dalf := ph*cosok + pinc*cosis*sinok;
             dbet := -ph*sinok + pinc*cosis*cosok;
             alfdp := alfdp + dalf;
             betdp := betdp + dbet;
             xls := xll + omgasm + cosis*xnodes;
             dls := pl + pgh - pinc*xnodes*sinis;
             xls := xls + dls;
             xnodes := Actan(alfdp,betdp);
             xll := xll + pl;
             omgasm := xls - xll - Cos(xinc)*xnodes;
        230: end; {dpper}
    end; {case}
  end; {Procedure Deep}

Procedure Call_dpinit(var eosq,sinio,cosio,betao,aodp,theta2,
                          sing,cosg,betao2,xmdot,omgdot,
                          xnodott,xnodpp : double);
  begin
  eqsq   := eosq;    siniq  := sinio;   cosiq  := cosio;   rteqsq := betao;
  ao     := aodp;    cosq2  := theta2;  sinomo := sing;    cosomo := cosg;
  bsq    := betao2;  xlldot := xmdot;   omgdt  := omgdot;  xnodot := xnodott;
  xnodp  := xnodpp;
  Deep(1);
  eosq   := eqsq;    sinio  := siniq;   cosio  := cosiq;   betao  := rteqsq;
  aodp   := ao;      theta2 := cosq2;   sing   := sinomo;  cosg   := cosomo;
  betao2 := bsq;     xmdot  := xlldot;  omgdot := omgdt;   xnodott := xnodot;
  xnodpp  := xnodp;
  end; {Procedure Call_dpinit}

Procedure Call_dpsec(var xmdf,omgadf,xnode,emm,xincc,xnn,tsince : double);
  begin
  xll    := xmdf;    omgasm := omgadf;  xnodes := xnode;   {_em     := emm;
  xinc   := xincc;}   xn     := xnn;     t      := tsince;
  Deep(2);
  xmdf   := xll;     omgadf := omgasm;  xnode  := xnodes;  emm    := _em;
  xincc  := xinc;    xnn    := xn;      tsince := t;
  end; {Procedure Call_dpsec}

Procedure Call_dpper(var e,xincc,omgadf,xnode,xmam : double);
  begin
  _em     := e;       xinc   := xincc;   omgasm := omgadf;  xnodes := xnode;
  xll    := xmam;
  Deep(3);
  e      := _em;      xincc  := xinc;    omgadf := omgasm;  xnode  := xnodes;
  xmam   := xll;
  end; {Procedure Call_dpper}

Procedure SDP4(tsince : double;
            var iflag : integer;
          var pos,vel : vector);
  label
    9,10,90,100,130,140;
  const
    a1     : double = 0;  a3ovk2 : double = 0;  ao     : double = 0;
    aodp   : double = 0;  aycof  : double = 0;  betao  : double = 0;
    betao2 : double = 0;  c1     : double = 0;  c2     : double = 0;
    c4     : double = 0;  coef   : double = 0;  coef1  : double = 0;
    cosg   : double = 0;  cosio  : double = 0;  del1   : double = 0;
    delo   : double = 0;  eeta   : double = 0;  eosq   : double = 0;
    eta    : double = 0;  etasq  : double = 0;  omgdot : double = 0;
    perige : double = 0;  pinvsq : double = 0;  psisq  : double = 0;
    qoms24 : double = 0;  s4     : double = 0;  sing   : double = 0;
    sinio  : double = 0;  t2cof  : double = 0;  temp1  : double = 0;
    temp2  : double = 0;  temp3  : double = 0;  theta2 : double = 0;
    theta4 : double = 0;  tsi    : double = 0;  x1m5th : double = 0;
    x1mth2 : double = 0;  x3thm1 : double = 0;  x7thm1 : double = 0;
    xhdot1 : double = 0;  xlcof  : double = 0;  xmdot  : double = 0;
    xnodcf : double = 0;  xnodot : double = 0;  xnodp  : double = 0;
  var
    i                                                  : integer;
    a,axn,ayn,aynl,beta,betal,capu,cos2u,cosepw,cosik,
    cosnok,cosu,cosuk,e,ecose,elsq,em,epw,esine,omgadf,
    pl,r,rdot,rdotk,rfdot,rfdotk,rk,sin2u,sinepw,sinik,
    sinnok,sinu,sinuk,temp,temp4,temp5,temp6,tempa,
    tempe,templ,tsq,u,uk,ux,uy,uz,vx,vy,vz,xinc,xinck,
    xl,xll,xlt,xmam,xmdf,xmx,xmy,xn,xnoddf,xnode,xnodek,
    x,y,z,xdot,ydot,zdot                               : double;
  begin
  if (iflag = 0) then
    goto 100;
{ Recover original mean motion (xnodp) and semimajor axis (aodp) }
{ from input elements. }
  a1 := Power(xke/xno,tothrd);
  cosio := cos(xincl);
  theta2 := cosio*cosio;
  x3thm1 := 3*theta2 - 1;
  eosq := eo*eo;
  betao2 := 1 - eosq;
  betao := sqrt(betao2);
  del1 := 1.5*ck2*x3thm1/(a1*a1*betao*betao2);
  ao := a1*(1 - del1*(0.5*tothrd + del1*(1 + 134/81*del1)));
  delo := 1.5*ck2*x3thm1/(ao*ao*betao*betao2);
  xnodp := xno/(1 + delo);
  aodp := ao/(1 - delo);
{ Initialization }
{ For perigee below 156 km, the values of s and qoms2t are altered. }
  s4 := s;
  qoms24 := qoms2t;
  perige := (aodp*(1 - eo) - ae)*xkmper;
  if (perige >= 156) then goto 10;
  s4 := perige - 78;
  if (perige > 98) then goto 9;
  s4 := 20;
  9:
  qoms24 := Power((120 - s4)*ae/xkmper,4);
  s4 := s4/xkmper + ae;
 10:
  pinvsq := 1/(aodp*aodp*betao2*betao2);
  sing := sin(omegao);
  cosg := cos(omegao);
  tsi := 1/(aodp - s4);
  eta := aodp*eo*tsi;
  etasq := eta*eta;
  eeta := eo*eta;
  psisq := abs(1 - etasq);
  coef := qoms24*Power(tsi,4);
  coef1 := coef/Power(psisq,3.5);
  c2 := coef1*xnodp*(aodp*(1 + 1.5*etasq + eeta*(4 + etasq))
      + 0.75*ck2*tsi/psisq*x3thm1*(8 + 3*etasq*(8 + etasq)));
  c1 := bstar*c2;
  sinio := sin(xincl);
  a3ovk2 := -xj3/ck2*Power(ae,3);
  x1mth2 := 1 - theta2;
  c4 := 2*xnodp*coef1*aodp*betao2*(eta*(2 + 0.5*etasq)
      + eo*(0.5 + 2*etasq) - 2*ck2*tsi/(aodp*psisq)
      *(-3*x3thm1*(1 - 2*eeta + etasq*(1.5 - 0.5*eeta))
      + 0.75*x1mth2*(2*etasq - eeta*(1 + etasq))*cos(2*omegao)));
  theta4 := theta2*theta2;
  temp1 := 3*ck2*pinvsq*xnodp;
  temp2 := temp1*ck2*pinvsq;
  temp3 := 1.25*ck4*pinvsq*pinvsq*xnodp;
  xmdot := xnodp + 0.5*temp1*betao*x3thm1
         + 0.0625*temp2*betao*(13 - 78*theta2 + 137*theta4);
  x1m5th := 1 - 5*theta2;
  omgdot := -0.5*temp1*x1m5th + 0.0625*temp2*(7 - 114*theta2 + 395*theta4)
          + temp3*(3 - 36*theta2 + 49*theta4);
  xhdot1 := -temp1*cosio;
  xnodot := xhdot1 + (0.5*temp2*(4 - 19*theta2)
          + 2*temp3*(3 - 7*theta2))*cosio;
  xnodcf := 3.5*betao2*xhdot1*c1;
  t2cof := 1.5*c1;
  xlcof := 0.125*a3ovk2*sinio*(3 + 5*cosio)/(1 + cosio);
  aycof := 0.25*a3ovk2*sinio;
  x7thm1 := 7*theta2 - 1;
 90:
  iflag := 0;
  Call_dpinit(eosq,sinio,cosio,betao,aodp,theta2,sing,cosg,
              betao2,xmdot,omgdot,xnodot,xnodp);
{ Update for secular gravity and atmospheric drag }
100:
  xmdf := xmo + xmdot*tsince;
  omgadf := omegao + omgdot*tsince;
  xnoddf := xnodeo + xnodot*tsince;
  tsq := tsince*tsince;
  xnode := xnoddf + xnodcf*tsq;
  tempa := 1 - c1*tsince;
  tempe := bstar*c4*tsince;
  templ := t2cof*tsq;
  xn := xnodp;
  Call_dpsec(xmdf,omgadf,xnode,em,xinc,xn,tsince);
  a := Power(xke/xn,tothrd)*Sqr(tempa);
  e := em - tempe;
  xmam := xmdf + xnodp*templ;
  Call_dpper(e,xinc,omgadf,xnode,xmam);
  xl := xmam + omgadf + xnode;
  beta := sqrt(1 - e*e);
  xn := xke/Power(a,1.5);
{ Long period periodics }
  axn := e*cos(omgadf);
  temp := 1/(a*beta*beta);
  xll := temp*xlcof*axn;
  aynl := temp*aycof;
  xlt := xl + xll;
  ayn := e*sin(omgadf) + aynl;
{ Solve Kepler's Equation }
  capu := fmod2p(xlt - xnode);
  temp2 := capu;
  for i := 1 to 10 do
    begin
    sinepw := sin(temp2);
    cosepw := cos(temp2);
    temp3 := axn*sinepw;
    temp4 := ayn*cosepw;
    temp5 := axn*cosepw;
    temp6 := ayn*sinepw;
    epw := (capu - temp4 + temp3 - temp2)/(1 - temp5 - temp6) + temp2;
    if (abs(epw - temp2) <= e6a) then goto 140;
130:
    temp2 := epw;
    end; {for i}
{ Short period preliminary quantities }
140:
  ecose := temp5 + temp6;
  esine := temp3 - temp4;
  elsq := axn*axn + ayn*ayn;
  temp := 1 - elsq;
  pl := a*temp;
  r := a*(1 - ecose);
  temp1 := 1/r;
  rdot := xke*sqrt(a)*esine*temp1;
  rfdot := xke*sqrt(pl)*temp1;
  temp2 := a*temp1;
  betal := sqrt(temp);
  temp3 := 1/(1 + betal);
  cosu := temp2*(cosepw - axn + ayn*esine*temp3);
  sinu := temp2*(sinepw - ayn - axn*esine*temp3);
  u := actan(sinu,cosu);
  sin2u := 2*sinu*cosu;
  cos2u := 2*cosu*cosu - 1;
  temp := 1/pl;
  temp1 := ck2*temp;
  temp2 := temp1*temp;
{ Update for short periodics }
  rk := r*(1 - 1.5*temp2*betal*x3thm1) + 0.5*temp1*x1mth2*cos2u;
  uk := u - 0.25*temp2*x7thm1*sin2u;
  xnodek := xnode + 1.5*temp2*cosio*sin2u;
  xinck := xinc + 1.5*temp2*cosio*sinio*cos2u;
  rdotk := rdot - xn*temp1*x1mth2*sin2u;
  rfdotk := rfdot + xn*temp1*(x1mth2*cos2u + 1.5*x3thm1);
{ Orientation vectors }
  sinuk := sin(uk);
  cosuk := cos(uk);
  sinik := sin(xinck);
  cosik := cos(xinck);
  sinnok := sin(xnodek);
  cosnok := cos(xnodek);
  xmx := -sinnok*cosik;
  xmy := cosnok*cosik;
  ux := xmx*sinuk + cosnok*cosuk;
  uy := xmy*sinuk + sinnok*cosuk;
  uz := sinik*sinuk;
  vx := xmx*cosuk - cosnok*sinuk;
  vy := xmy*cosuk - sinnok*sinuk;
  vz := sinik*cosuk;
{ Position and velocity }
  x := rk*ux;  pos[1] := x;
  y := rk*uy;  pos[2] := y;
  z := rk*uz;  pos[3] := z;
  xdot := rdotk*ux + rfdotk*vx;  vel[1] := xdot;
  ydot := rdotk*uy + rfdotk*vy;  vel[2] := ydot;
  zdot := rdotk*uz + rfdotk*vz;  vel[3] := zdot;
  end; {Procedure SDP4}

Procedure SGP(time : double;
       var pos,vel : vector);
  var
    tsince : double;
  begin
  tsince := (time - julian_epoch) * xmnpda;
  if ideep = 0 then
    SGP4(tsince,iflag,pos,vel)
  else
    SDP4(tsince,iflag,pos,vel);
  end; {Procedure SGP}

  begin
  Define_Derived_Constants;

end.
