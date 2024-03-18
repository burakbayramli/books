% check spanloading vs. sweep and taper
% make quad wing from area A, aspect ratio AR,
% c.25-sweep SW, taper T
% only one
close all
clear all
clc
%

avel = 20
state.AS     = avel;
state.P      =0;
state.Q      =0;
state.R      =0;
state.alpha  = 4*pi/180;
state.betha  = 0;
state.pgcorr = 0;
state.rho    = 1.273;
geo.fnx =  [0 ]
geo.ny =  [20]
geo.nx =  [10]
geo.fsym =  [0]
geo.fc =  [0]
geo.flapped =  [0]
geo.TW(1,1,:) = [0 0] %[1x5x2 double]
geo.foil(1,1,1) = {'4412'};
geo.foil(1,1,2) = {'4412'};
geo.dihed = [0]
geo.symetric = 0
geo.startx  = 0
geo.starty = 0
geo.startz = 0
geo.nwing = 1
geo.nelem = 1
geo.flap_vector = [0 ]
geo.ref_point = [0 0 0]
geo.CG = [0 0 0]
% fixed
AR = 4
A  = 1
b = sqrt(A*AR);
geo.b = [b]

% A = bc(1+t)/2
% AR = b^2/A

swlist = [0 10 20 30 40 50]*pi/180;
tlist  = [0.2 0.4 0.6 0.8 1];
for sw = swlist
    for t = tlist
        c = A/(b*(1+t)/2);  
        geo.T = [t]
        geo.SW = [sw]
        geo.c = c

        [lattice,ref]=fLattice_setup2(geo,state,0);
        figure(1)
        g=fill3(lattice.XYZ(:,:,1)',lattice.XYZ(:,:,2)',lattice.XYZ(:,:,3)','w');
        set(g,'LineWidth',2);
        axis equal
        hold on
        title([num2str(avel)])
        results = [];
        % cfd
        'solver9'
        [results]=solver9(results,state,geo,lattice,ref);
        'coeff3'
        [results]=coeff_create3(results,lattice,state,ref,geo);
        if sw == swlist(3)
            figure(2)
            plot(results.ystation/b,results.ForcePerMeter,'.-k')
            title(['sw ',num2str(sw*180/pi)])
            hold on
        end
        if t == tlist(3)
            figure(3)
            plot(results.ystation/b,results.CL_local,'.-k')
            title(['t ',num2str(t)])
            hold on
        end
        
    end
end
