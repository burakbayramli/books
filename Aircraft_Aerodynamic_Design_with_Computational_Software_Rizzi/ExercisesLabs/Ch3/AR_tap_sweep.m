%
close all
clear all
format compact
foildir        = 'C:\Users\jespe\Desktop\fromubuntu\wingapp2\foil';
foillibdir     = 'C:\Users\jespe\Desktop\fromubuntu\wingapp2\foillib';
thomedir       = 'C:\Users\jespe\Desktop\PyTor\T136_2_export';
homedir        = pwd
ii = strfind(homedir,filesep)
vlmlib = [homedir filesep 'VLMdir']
thomedir = [homedir(1:ii(end)) filesep 'T136_2_export']

%rmpath (foillibdir);
%addpath(foillibdir);
%rmpath (foildir);
%addpath(foildir);
rmpath (thomedir);
addpath(thomedir);
rmpath(vlmlib);
addpath(vlmlib);
output = config2('startup',thomedir,homedir,foildir);
% vary
% Mach          Minf  
% AoA           alpha
% Sweep         QCsweep (quarter chord)
% Taper ratio   taprat  
% Aspect ratio  AR
% Tip twist     twisttip
% Dihedral      dihed
% output
% CL, CD
% cll

AR   = 11
deg  = pi/180;
dihed   = 0 % 7
foilin  = 'rae100' % symmetric
foilout = 'rae100'
Qcsweep = 0*deg
alt      = 3000 % m
airspeed = 130 % m/s
alpha    = (-2:1:5)*deg %
np = length(alpha);
[rho,a,p,mu]=ISAtmosphere(alt);
q         = 1/2*rho*airspeed^2;
% AR = b*b/S
Minf = airspeed/a;
rootcenter = [0,0,0];
tapratlist = [0.5 ];
twisttiplist = [0 ]*deg;
CLst = 0.36
CDtab  = zeros(length(tapratlist),length(twisttiplist));
for kt = 1:length(tapratlist)
    taprat= tapratlist(kt);
    for kw = 1:length(twisttiplist)
        twisttip = twisttiplist(kw);
        [CL,CD,CL_a,resstrct,lattice,ref,geo] = torn_one(AR,taprat,Qcsweep/deg,dihed/deg,twisttip/deg,...
            alt,airspeed,alpha/deg,foilin,foilout);
        % interpolate to given CL = CLst
        alfst = interp1(CL,alpha/deg,CLst);
        CDst  = interp1(alpha/deg,CD,alfst);
        % span efficiency etc,
        coe = polyfit(CL,CD,2);
        CD0  = coe(3);
        eOst = 1/(coe(1)*pi*AR);
        coe  = polyfit(alpha,CL,2);
        CL_ao2pi = coe(2)/(2*pi);
        cl_ao2pi = 1/(1+2/AR);
        fprintf('CL_a/2pi  theo.     CD0       eOst \n')
        fprintf('----------------------------------\n')
        fprintf('%7.3f  %7.3f %7.3f %7.3f \n',CL_ao2pi, cl_ao2pi,CD0,eOst)
        CDtab(kt,kw)=CDst;
        %
        % Drag polar 
%         subplot(211)
%         hold on
%         pldragpolar(CL,CD,alpha/deg)
        
       % Lift distribution
       % subplot(212)
        
        subplot(211)
        hold on
        quad  = plotplnfrm(AR,taprat,Qcsweep,dihed,twisttip);
        cll = resstrct.CL_local;
        yst = resstrct.ystation;
        nst = size(yst,1);
        nsto2 = round(nst/2);
        tmp = yst(nsto2+1:end);
        ys  = zeros(nsto2+1,1);
        for k = 2:nsto2+1
            ys(k) = 2*yst(nsto2+k-1)-ys(k-1);
        end
        ys = [-flipud(ys(2:end));ys];
        dy = diff(ys);
        plot([quad(2,:),quad(2,1)],[quad(1,:),quad(1,1)],'k','linewidth',1)
       %patch(quad(2,:),quad(1,:),'c')
        plot(yst,cll,'-k','linewidth',2)
        plot(yst,max(cll)*1.1*ones(size(yst)),'--k')
        %legend('shape','clloc')
        title('Shape, c_{l,loc}(y), and c_{l,max}','fontsize',14)
        xlabel('y','fontsize',14)
        ylabel('cl(y)','fontsize',14)  
        axis equal
        
        subplot(212)
        hold on
        [~,cc] = lattic2chord(lattice,geo);
        plot(yst,cll.*cc{1}/(resstrct.CL*ref.C_mac),'-k','linewidth',2)
        L = sum(cll.*cc{1}.*dy)/(resstrct.CL*ref.C_mac);
        elcl = 2*L/(pi*max(ys))*sqrt(1-(ys/max(ys)).^2);
        plot(ys,elcl,'--k','linewidth',2)
        %legend('clloc*c/(CL MAC)','Ellip')
        title(['Lift distribution, M_{\infty}: ',sprintf('%5.2f',Minf)],'fontsize',14)
        xlabel('y','fontsize',14)
        ylabel(' cl(y)*c(y)/(CL MAC)','fontsize',14)   
        axis equal
    end
end



