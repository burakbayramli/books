function Tm = Tmoist(p,z, nl, zlfc, plfc, Tmlfc);
% Matlab function to return pseudoadiatic profile given temperature
% pressure and height of level of free convection.
% Algorithm compares thetae at lfc to thetaes at each level and adjusts
% temperature until there is a match.
% p and z are vectors giving pressure and height at nl levels of sounding

% Define physical constants.
g = 9.81;                   % gravity
R = 287;                    % gas constant for dry air
cp = 1004;                  % specific heat at constant press
Ttr = 273.16;               % triple point temperature
Lv  = 2.5e6;                % latent heat of vaporization
estr = 6.11;                % saturation vapor pressure at triple point (hPa)
Rv  = 461;                  % gas constant for vapor
eps = 0.622;                % ratio of water vapor mass to dry air mass
% compute thataes at LFC
thetalfc = Tmlfc*(1000/plfc)^(R/cp);
esatlfc= estr*exp(Lv/Rv*(1/Ttr-1./Tmlfc));  % saturation vapor pressure at LFC
qvslfc = eps*esatlfc/plfc;                  % saturation mixing ratio at LFC
thetaeslfc = thetalfc*exp(Lv.*qvslfc/(cp*Tmlfc)); %(Eq. 9.40 in text)
% make initial guess for moist adiabat
Tm = Tmlfc - 6.e-3*(z-zlfc);
% compute first guess for potential temperature 
theta = Tm.*(1000./p).^(R/cp);
% compute first guess saturation vapor pressure 
esat= estr*exp(Lv/Rv*(1/Ttr-1./Tm));        % saturation vapor pressure
qvs = eps*esat./p;                          % saturation mixing ratio
% compute saturation equivalent potential temperature 
thetaes = theta.*exp(Lv.*qvs./(cp.*Tm));
% Iterate to get converged Tm
for j=1:nl
    crit =0.1 ;  % convergence criterion
    while(abs(thetaes(j)-thetaeslfc)>crit);
        delTm = (p(j)/1000)^(R/cp)*(thetaes(j)-thetaeslfc)*.1;
        Tm(j) = Tm(j) -delTm;
        theta(j) = Tm(j).*(1000./p(j)).^(R/cp);
        esat(j) = estr*exp(Lv/Rv*(1/Ttr-1./Tm(j)));  % saturation vapor pressure
        qvs(j) = eps*esat(j)/p(j);         
        thetaes(j) = theta(j)*exp(Lv.*qvs(j)/(cp.*Tm(j)));
    end
end