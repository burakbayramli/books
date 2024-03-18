function tab = naca45prof(foil)
% Tornado function
%  KTH 0911 - 4 and 5 digit NACA
%------------------------------------
np  = 100;
npu = np;
npl = np; % same # points on upp and low
x   = linspace(0,1,np)';
yu1 = (0.2969*sqrt(x)-x.*(0.1260 + x.*(0.3516 - x.*(0.2843 -0.1015*x))));

if foil < 9999 % 4-digit
    m     = fix(foil/1000);	%gives first NACA-4 number
    lemma = foil-m*1000;
    p     = fix(lemma/100);	%gives second NACA-4 number
    lemma = lemma - p*100;   % two last digits
    t = lemma/100;
    p = p/10;
    m = m/100;
    yu = t/0.2*yu1;
    yl = -yu;
    if m == 0
        yc    = zeros(np,1);
        dycdx = zeros(np,1);
    else
        yc = m*x.*(2*p-x)/p^2;
        dycdx = m*(2*p-2*x)/p^2;
        ii = find(x>p);
        yc(ii)=m/(1-p)^2*(1-x(ii)).*(1 + x(ii)-2*p);
        dycdx(ii) = m/(1-p)^2*(2*p-2*x(ii));
    end
    
elseif foil < 99999 % 5-digit
    d1 = fix(foil/10000);	%gives first NACA-5 number: max camber
    d2 = foil-d1*10000;
    p  = fix(d2/100);
    t  = (d2-p*100)/100;
    p  = p/200;	%gives p
    % y = k1/6(x^3-3mx^2 +m^2(3-m)x), x < m
    %   = k1/6 m^3(1-x) m such that max y at x = p
    % p = m(1-sqrt(m/3))
    % find m
    m = 0.5;
    for k = 1:10
        mn = m - (m-sqrt(m^3/3)-p)/(1-1.5/sqrt(3)*sqrt(m));
        if abs(mn - m) < 1e-5;
            break
        end
        m = mn;
    end
    ymax = (p-m)^3+m^3-m^3*p;
    % k1*ymax/6 = d1/100
    k16 = d1/100/ymax;
    iip = x>m;
    yc         = k16*(x.^3-3*m*x.^2+3*m^2*x-m^3*x);
    dycdx      = k16*(3*x.^2-6*m*x+3*m^2-m^3);
    yc(iip)    = k16*m^3*(1-x(iip));
    dycdx(iip) = k16*m^3;
else
    disp(' foil: ',num2str(foil),' not 4 or 5 digit, plate returned')
    yc    = zeros(np,1);
    dycdx = zeros(np,1);
end
the = atan(dycdx);
xu = x  - yc.*sin(the);
yu = yc + yu.*cos(the);
xl = x  + yc.*sin(the);
yl = yc - yc.*cos(the);
tab= [[npu;xu;xl],[npl;yu;yl]];
