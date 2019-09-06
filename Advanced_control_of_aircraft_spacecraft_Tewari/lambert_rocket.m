function deriv=lambert_rocket(t,o);
  global hf; global delf; global lamf;
  global mu; global omega; global S; global c;
  global R0; global tb1; global tb2; global fT1;
  global fT2; global m01; global m02;
  global mL; global mp1; global mp2; global Gamma;
  global machr; global Cdr;
  [g,gn]=gravity(o(3),o(2)); %accln. due to gravity (oblate earth)
  lo = o(1);la = o(2);
  cLH=CLH(la,lo);
  clo = cos(lo); slo = sin(lo); cla = cos(la); sla = sin(la);
  fpa = o(5); chi = o(6); cfpa = cos(fpa); sfpa = sin(fpa);
  cchi = cos(chi); schi = sin(chi);
%%%atmospheric drag determination
  if o(3)<R0; o(3)=R0; end
  alt = o(3) - R0;
  v = o(4);
  if v<0; v=0; end
  if alt<=2000e3
    atmosp = atmosphere(alt,v,c);
    rho = atmosp(2);
    Qinf = 0.5*rho*v^2;
    mach = atmosp(3);
    Kn=atmosp(4);
    CDC=interp1(machr, Cdr, mach);
    s = mach*sqrt(Gamma/2);
    CDFM=1.75+sqrt(pi)/(2*s);
    iflow=atmosp(6);
    if iflow==2
      CD=CDC;
    elseif iflow==1
      CD=CDFM;
    else
      CD = CDC + (CDFM - CDC)*(0.333*log10(Kn/sin(pi/6))+0.5113);
    end
  else
    rho=0;Qinf=0;CD=0;mach=0;
  end
  if t<=tb1
    fT=fT1; m=m01-mp1*t/tb1; CD=8*CD;
  elseif t<=(tb1+tb2)
    fT=fT2; m=m02-mp2*(t-tb1)/tb2; CD=3*CD;
  else
    fT=0; m=mL;
  end
  D=Qinf*S*CD;
  Xfo = fT-D; Yfo = 0; Zfo = 0;
  R=o(3)*[cla*clo cla*slo sla]';
  V=v*[sfpa cfpa*schi cfpa*cchi]';
  Rf=(hf+R0)*[cos(delf)*cos(lamf) cos(delf)*sin(lamf) sin(delf)]';
  if alt>100e3 && t<tb1+tb2-.1
    [a,p,Vi]=lambert_hypergeom(mu,R,Rf,tb1+tb2-t,-1);
    Vi=cLH*Vi; %planet-fixed to local horizon frame
    u=-(Vi-V)/v;
  else
    u=zeros(3,1);
  end
				      %trajectory equations follow :
  longidot = o(4)*cfpa*schi/(o(3)*cla); %Longitude
  latidot = o(4)*cfpa*cchi/o(3); %Latitude
  raddot = o(4)*sfpa; %Radius
  veldot = -g*sfpa +gn*cchi*cfpa + Xfo/m + ...
	   omega*omega*o(3)*cla*(sfpa*cla - cfpa*cchi*sla);
  if t<=10; headdot=0; gammadot=0;
  else
    gammadot = u(1,1)+(o(4)/o(3)-g/o(4))*cfpa+gn*cchi*sfpa/o(4) ...
	       + Zfo/(o(4)*m) + 2*omega*schi*cla ...
	       + omega*omega*o(3)*cla*(cfpa*cla + sfpa*cchi*sla)/o(4);
    if abs(cfpa)>1e-6
      headdot = u(2,1)+ o(4)*schi*tan(o(2))*cfpa/o(3)-gn*schi/o(4) ...
		-Yfo/(o(4)*cfpa*m)-2*omega*(tan(o(5))*cchi*cla-sla) ...
		+omega*omega*o(3)*schi*sla*cla/(o(4)*cfpa);
    else
      headdot=0;
    end
  end
  deriv = [longidot; latidot; raddot; veldot; gammadot; headdot];
