%
close all
clear all
format compact
%foildir        = 'C:\Users\jespe\Desktop\fromubuntu\wingapp2\foil';
%foillibdir     = 'C:\Users\jespe\Desktop\fromubuntu\wingapp2\foillib';
%thomedir       = 'C:\Users\jespe\Desktop\PyTor\T136_2_export';
homedir        = pwd
ii = strfind(homedir,filesep)
vlmlib = [homedir filesep 'VLMlib']

thomedir = [homedir(1:ii(end)) filesep 'T136_2_export'];
foildir = [thomedir filesep 'aircraft' filesep 'airfoil'];

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
% ARlist,
% tiptaperlist
% sweeplist,
% dihedrallist,
% twistlist
% alphalist

ARlist       = [2, 4, 6, 8, 10];
tiptaperlist = [1]
sweeplist    = [0]
dihedrallist = [0]
twistlist    = [0]
alphalist     = (-2:1:5)*pi/180 %
i = 0
for iAR = 1:length(ARlist)
  AR = ARlist(iAR);
  for itap = 1:length(tiptaperlist)
    tap = tiptaperlist(itap);
    for iswe = 1:length(sweeplist)
      swe = sweeplist(iswe);
      for idih = 1:length(dihedrallist)
        dih = dihedrallist(idih);
        for itwi = 1:length(twistlist)
          twi = twistlist(itwi);
          for ial = 1:length(alphalist)
            al = alphalist(ial);
            i = i+1;
            partab(i,1) = AR;
            partab(i,2) = tap;
            partab(i,3) = swe;
            partab(i,4) = dih;
            partab(i,5) = twi;
            partab(i,6) = al;
          end
        end
      end
    end
  end
end
nsim = i;
deg     = pi/180;
foilin  = 'naca0012' % symmetric
foilout = 'naca0012'
alt      = 3000 % m
airspeed = 80 % m/s

[rho,a,p,mu]=ISAtmosphere(alt);
q         = 1/2*rho*airspeed^2;
% AR = b*b/S,
Minf = airspeed/a;
rootcenter = [0,0,0];
resimn = 2
resimx = 4
for i = 1:nsim
  AR  = partab(i,1);
  tap = partab(i,2);
  swe = partab(i,3);
  dih = partab(i,4);
  twi = partab(i,5);
  al  = partab(i,6);
  partab(i,:)
 [coeffs] = ...
               tor1(AR,tap,swe,dih,twi,...
               alt,airspeed,al,foilin,foilout,resimn,resimx)
  restab(i,1:6) = coeffs;
end

