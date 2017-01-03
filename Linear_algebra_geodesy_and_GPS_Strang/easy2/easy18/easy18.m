% EASY18  Read RINEX navigation file reformat into Matlab Eph matrix.
%         Open a RINEX observation file, analyse the header and identify
%         observation types. The function fepoch_0 finds epoch time
%         and observed PRNs in an OK epoch (digit 0, RTK observations
%         will have a 2 in this place). Next we read the observations
%         and use recpo_ls to get a least-squares estimate for the
%         (stand alone) receiver position.

%Kai Borre 13-06-2008
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 2008/06/13  $

set(0,'DefaultTextFontName','Times');
set(0,'DefaultAxesFontName','Times');
set(0,'DefaultTextFontSize',16);

v_light = 299792458;
dtr = pi/180;

% Read RINEX ephemerides file and convert to
% internal Matlab format
rinexe('SITE247J.01N','eph.dat');
Eph = get_eph('eph.dat');
% open master observation file
ofile1 = 'SITE247J.01O';
fid1 = fopen(ofile1,'rt');
[Obs_types1] = anheader(ofile1);
NoObs_types1 = size(Obs_types1,2)/2;

% There are 23 epochs of data in ofile1
qend = 23;
pos = zeros(4,qend);

for q = 1:qend
    [time1, dt1, sats1, eof1] = fepoch_0(fid1);
    NoSv1 = size(sats1,1);
    % We pick observed pseudoranges
    obs1 = grabdata(fid1, NoSv1, NoObs_types1);
    i = fobs_typ(Obs_types1,'C1'); % possible choices: C1, P1, or P2
    pos(:,q) = recpo_lsa(obs1(:,i),sats1,time1,Eph); 
end
% we close all open files and re-open for reading 
% from the beginning
fclose all;
% we compute the means vaues of X, Y, Z; 
POS = mean(pos,2);

ofile1 = 'SITE247J.01O';
fid1 = fopen(ofile1,'rt');
[Obs_types1, ant_delta1, ifound_types1, eof11] = anheader(ofile1);
NoObs_types1 = size(Obs_types1,2)/2;
DIFF = [];

for q = 1:qend
    [time1, dt1, sats1, eof1] = fepoch_0(fid1);
    NoSv1 = size(sats1,1);
    obsm = grabdata(fid1, NoSv1, NoObs_types1);
    i = fobs_typ(Obs_types1,'C1');
    obs1 = obsm(:,i);
    Diff = [];
    obs = zeros(NoSv1,1);
    for s = 1:NoSv1
        col_Eph = find_eph(Eph,sats1(s),time1);
        for iter = 1:3
            k = col_Eph;
            tx_RAW = time1 - obs1(s)/v_light;
            t0c = Eph(21,k);
            dt = check_t(tx_RAW-t0c);
            tcorr = (Eph(2,k)*dt + Eph(20,k))*dt + Eph(19,k);
            tx_GPS = tx_RAW-tcorr;
            dt = check_t(tx_GPS-t0c);
            tcorr = (Eph(2,k)*dt + Eph(20,k))*dt + Eph(19,k);
            tx_GPS = tx_RAW-tcorr;
            X = satpos(tx_GPS, Eph(:,k));
            if iter == 1
                traveltime = 0.072;
                Rot_X = X;
                trop = 0;
            else
                rho2 = (X(1)-POS(1))^2+(X(2)-POS(2))^2+(X(3)-POS(3))^2;
                traveltime = sqrt(rho2)/v_light;
                Rot_X = e_r_corr(traveltime,X);
                [az,el,dist] = topocent(POS(1:3),Rot_X-POS(1:3));
                if iter == 3, El(i) = el; end
                trop = tropo(sin(el*dtr),0.0,1013.0,293.0,50.0,0.0,0.0,0.0);
                % POS(4) is the clock offset for the entire period. However, we
                % need the clock offset for each epoch q as given by
                % pos(4,q)
                rho = norm(Rot_X-POS(1:3),'fro') + pos(4,q) - v_light*tcorr + trop;
            end
        end % iter
        diff0(s,1) = rho-obs1(s);
    end % s
    DIFF = [DIFF, diff0];
end % q

figure(1);
subplot(1,2,1), plot(DIFF','linewidth',1)
set(gca,'Fontsize',16); 
xlabel('Epochs [1 s]') 
ylabel('Range Correction [m]')

subplot(1,2,2), plot(diff(DIFF,1,2)','linewidth',1)
set(gca,'Fontsize',16); 
xlabel('Epochs [1 s]') 
ylabel('Range Rate Correction [m/s]')

print -depsc easy18
%print -deps easy18
%%%%%%%%%%%%%%%%%%%%% end easy18.m %%%%%%%%%%%%%%%



