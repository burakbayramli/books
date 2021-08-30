f=[1e3, 1e6]; % frequencies
omega=2*pi*f; % angular frequencies
eta=1e-3; % shear viscosity
k=2.2e9;  % bulk modulus
rho=1000;  % density in kg/m^3
gstar=1i*omega*eta; % complex shear modulus
elstar=k+1.333*gstar;  % complex longitudinal elastic modulus
kstar=omega.*(rho./elstar).^0.5;  % complex wave number
l=-1./imag(kstar);  % wavelength
lambda=2*pi./real(kstar);  % decay length


