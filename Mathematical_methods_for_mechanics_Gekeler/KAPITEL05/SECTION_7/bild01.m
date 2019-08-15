function bild01
% Figure to Example of Allgower/Georg

disp(' Call first DEMO1-4 or DEMO2-4 ')
clf
load daten4 WEG
[M,N] = size(WEG);
ABSZISSE = WEG(M,:);
ORDINATE = zeros(1,N);
for I = 1:N
    ORDINATE(I) = norm(WEG(1:4,I));
end
plot(ABSZISSE,ORDINATE,'k','linewidth',2), hold on
axis equal, grid on
