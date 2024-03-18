close all
clear all
format compact
%
filnm = 'balsa.txt';
fid   = fopen(filnm,'r');
nfuse = 0;
nwing = 0;
figure(1)
hold on
while 1
  ll    = fgetl(fid);
  if ll == -1
    break
  end
  if strcmp(ll(3:6),'fuse')
    nfuse = nfuse + 1; 
    ll = fgetl(fid);
    np = sscanf(ll,'%f')
    tmp = zeros(np,3);
    for k = 1:np
      ll = fgetl(fid)
      tmp(k,:) = sscanf(ll,'%f %f %f');
    end
    fusexyz{nfuse}=tmp;
  elseif strcmp(ll(3:6),'wing')
    nwing = nwing +1;
    ll = fgetl(fid);
    np = sscanf(ll,'%f')
    tmp = zeros(np,3);
    for k = 1:np
      ll = fgetl(fid)
      tmp(k,:) = sscanf(ll,'%f %f %f');
    end
    wingxyz{nwing}=tmp;
  end
end
fclose(fid)
for k = 1:nfuse
  tmp = fusexyz{k};
  plot3(tmp(:,1),tmp(:,2),tmp(:,3),'o-k','linewidth',2);
  tmpm = mean(tmp);
  text(tmpm(1),tmpm(2),tmpm(3),['fuse ' num2str(k)],'fontsize',18);
end
for k = 1:nwing
  tmp = wingxyz{k};
  plot3(tmp(:,1),tmp(:,2),tmp(:,3),'o-k','linewidth',2);
  tmpm = mean(tmp);
  text(tmpm(1),tmpm(2),tmpm(3),['wing ' num2str(k)],'fontsize',18);
end
axis equal
xlabel('x')
ylabel('y')
zlabel('z')
title(filnm,'fontsize',18)
set(gca,'fontsize',18)
