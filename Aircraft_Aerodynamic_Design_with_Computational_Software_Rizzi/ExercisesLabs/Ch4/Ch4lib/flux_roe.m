%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
%                         DEMOFLOW                                  %
%                                                                   %
%       CALCULATION OF QUASI-1D INVISCID FLOW IN A TUBE/NOZZLE      %
%       ======================================================      %
%             Alexander von Essen, Created Apr. 5 2005              %
%                   Last modified: May 10 2005                      %
%                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
% Name: flux_roe.m                                                  %
% Purpose: Calculates convective flux and dissipation using         %
%          Roe's flux-difference splitting scheme.                  %
%          Source term added later.                                 %
% Called by: solveexplicit.m                                        %
% Calls: entropy_correction.m                                       %
%                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [handles] = flux_roe(handles)
ib2 = handles.Data.Nodes - 1;
a = handles.Data.a;
p = handles.Data.p;
gamma = handles.Data.gamma;
ls = handles.Data.ls;
rs = handles.Data.rs;
epsentr = handles.Data.epsentr;

nnodes=ib2+1;

fdiss = zeros(nnodes,3);
fcav  = zeros(nnodes,3);

si = 0.5.*(a(2:end) + a(1:ib2));

rl  = ls(1:ib2,1);
ul  = ls(1:ib2,2);
pl  = ls(1:ib2,3);
hl  = (gamma/(gamma-1)).*(pl./rl) + 0.5.*ul.^2;
qrl = ul.*rl;

rr  = rs(1:ib2,1);
ur  = rs(1:ib2,2);
pr  = rs(1:ib2,3);
hr  = (gamma/(gamma-1)).*(pr./rr) + 0.5.*ur.^2;
qrr = ur.*rr;

fcav(1:ib2,1) = qrl + qrr;
fcav(1:ib2,2) = qrl.*ul + qrr.*ur + pl + pr;
fcav(1:ib2,3) = qrl.*hl + qrr.*hr;

rav = sqrt(rl.*rr);
dd  = rav./rl;
dd1 = 1./(1+dd);

uav = (ul+dd.*ur).*dd1;
hav = (hl+dd.*hr).*dd1;
q2a = 0.5.*uav.^2;
c2a = (gamma-1).*(hav-q2a);
cav = sqrt(c2a);
du_b  = ur - ul;

% Eigenvalues of the Roe matrix.
h1  = abs(uav - cav);
h2  = abs(uav);
h3  = abs(uav + cav);

%%% New block %%%
% newbl = 0;
% if newbl
%     h1 = [];
%     h2 = [];
%     h3 = [];
%     
%     %save AA AA
%     
%     for n = 1:length(AA)
%         D = eig(AA(:,:,n));
%         D = sort(abs(D));
%         %   D = abs(D);
%         h1 = [h1; D(1)];
%         h2 = [h2; D(2)];
%         h3 = [h3; D(3)];
%     end
% end

delta = epsentr.*h3;

eabs1 = entropy_correction(h1,delta);
eabs2 = entropy_correction(h2,delta);
eabs3 = entropy_correction(h3,delta);

h1 = rav.*cav.*du_b;
% JO 1905 -
h2 = eabs1.*(pr-pl-h1)./(2.*c2a);
h3 = eabs2.*(rr-rl-(pr-pl)./c2a);
h5 = eabs3.*(pr-pl+h1)./(2.*c2a);
jos = 1;
fdiss(1:ib2,1) = h2 + h3 + jos*h5;
fdiss(1:ib2,2) = h2.*(uav-cav) + h3.*uav + jos*h5.*(uav+cav);
%fdiss(1:ib2,3) = h2.*(hav-cav.*uav) + h3.*q2a + h5.*(hav+cav+uav);
% JO 1905                                               ----V----
fdiss(1:ib2,3) = h2.*(hav-cav.*uav) + h3.*q2a + jos*h5.*(hav+cav.*uav);


du(1:ib2,1) = 0.5.*(fcav(1:ib2,1)-fdiss(1:ib2,1)).*si;
du(1:ib2,2) = 0.5.*(fcav(1:ib2,2)-fdiss(1:ib2,2)).*si;
du(1:ib2,3) = 0.5.*(fcav(1:ib2,3)-fdiss(1:ib2,3)).*si;

rhs=zeros(nnodes,3);

rhs(2:ib2,1) = diff(du(1:ib2,1));
rhs(2:ib2,2) = diff(du(1:ib2,2));
rhs(2:ib2,3) = diff(du(1:ib2,3));

handles.Data.rhs = rhs;
