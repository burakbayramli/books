function xyfoil =intrpfoil(xyflo,clo,xyfhi,chi,fr)
% linear intrp. to produce foil
nulo = xyflo(1,1);
nllo = xyflo(1,2);
nuhi = xyfhi(1,1);
nlhi = xyfhi(1,2);
s   = (1-fr)*clo+fr*chi;
clo = clo/s;
chi = chi/s;
if nulo > nuhi
    nu = nulo;
    xu = xyflo(2:nulo+1,1);
    yu = interp1(xyfhi(2:nuhi+1,1),xyfhi(2:nuhi1+1,2),xu,'spline');
    yu = (1-fr)*xyflo(2:nulo+1,2)*clo+fr*yu*chi;
else
    nu = nuhi;
    xu = xyfhi(2:nuhi+1,1);
    yu = interp1(xyflo(2:nulo+1,1),xyflo(2:nulo+1,2),xu,'spline');
    yu = (1-fr)*yu*clo+fr*xyfhi(2:nuhi+1,2)*chi;
end
if nllo > nlhi
    nl = nllo;
    xl = xyflo(nllo+2:end,1);
    yl = interp1(xyfhi(nlhi+2:end,1),xyfhi(nlhi+2:end,2),xl,'spline');
    yl = (1-fr)*xyflo(nllo+2:end,2)*clo+fr*yl*chi;
else
    nl = nlhi;
    xl = xyfhi(nlhi+2:end,1);
    yl = interp1(xyflo(nllo+2:end,1),xyflo(nllo+2:end,2),xl,'spline');
    yl = (1-fr)*yl*clo+fr*xyfhi(nlhi+2:end,2)*chi;
end
xyfoil = [[nu;xu;xl],[nl;yu;yl]];