function[samp]=gauss(ngp)
%
% This function returns the abscissas and weights of the Gauss points for ngp equal up to 4
%
%
samp=zeros(ngp,2);
if ngp==1
    samp=[0.  2];
elseif ngp==2
    samp=[1./sqrt(3)   1.;...
          -1./sqrt(3)  1.];
elseif ngp==3
    samp= [.2*sqrt(15.)   5./9; ...
            0.            8./9.;...
           -.2*sqrt(15.)   5./9];
elseif ngp==4
    samp= [0.861136311594053       0.347854845137454; ...
           0.339981043584856       0.652145154862546; ...
          -0.339981043584856       0.652145154862546; ...
          -0.861136311594053       0.347854845137454]; 
end 
% 
% End function Gauss