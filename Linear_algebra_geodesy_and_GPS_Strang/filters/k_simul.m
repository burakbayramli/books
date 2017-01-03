%K_SIMUL  Plots characteristics of a Kalman Filter and
%         covariance matrices.

%Kai Borre 02-22-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/10/15  $

e = exist('k_simul1.eps');
if e ~= 0, delete('k_simul1.eps'), end

e = exist('k_simul2.eps');
if e ~= 0, delete('k_simul2.eps'), end

b = [...
      31.82134 30.89820 38.23111 48.00730 56.65535 47.28452 55.75275 43.35551 ...
      23.79064 18.49350 26.89422 27.33513 26.45778 26.39760 34.20747 32.26430 ...
      56.55124 55.62368 67.82648 69.36661 71.39495 63.36397 60.44408 55.24069 ...
      59.54250 34.68445 47.65001 47.33768 51.08093 77.27994 86.58618 81.02339 ...
      73.85641 91.57223 69.38181 65.15144 47.73428 32.73260 36.51434 43.13130 ...
      48.39955 51.52907 60.83930 68.67321 79.08453 83.08164 85.20873 63.06489 ...
      66.51476 56.16452 57.36598 55.59506 55.99536 57.31815 58.38237 57.72976 ...
      57.94576 59.17708 57.29111 57.57266 59.41627 59.22560 55.81318 58.00549 ...
      58.94421 58.71120 57.39917 57.82708 56.45232 60.03749 58.72744 59.43749 ...
      60.00184 58.34971 58.30851 64.23442 59.02359 71.01284 64.71382 57.32984 ...
      57.88061 50.62056 52.60293 58.82162 61.33038 60.32829 72.24498 78.39212 ...
      48.50817 54.12422 70.07233 59.44640 59.21350 57.34199 77.77867 78.21832 ...
      85.76715 73.33420 72.75157 53.44409 65.76904 46.81751 50.93625 65.26724 ...
      71.19030 61.95633 52.89290 62.97470 71.69265 49.57475 60.16783 43.18437 ...
      58.64396 61.62705 49.65742 60.53506 63.67103 68.60288 75.52313 64.89241 ...
      65.50147 51.10454 62.29238 56.82327 59.13079];

x_acc = [];
K_acc = [];
P_acc = [];
conf_acc = [];
P = 0.001;	      % filtering variance
x = 25;
for epoch = 1:25
   [x, P, K, conf] = k_updatx(x, P, 1, b(epoch), 1, 100);
   x_acc = [x_acc; x];
   K_acc = [K_acc; K];
   P_acc = [P_acc; P];
   conf_acc = [conf_acc; 2*sqrt(conf)];
end
for epoch = 26:50
   [x, P, K, conf] = k_updatx(x, P, 1, b(epoch), 1, 0.5);
   x_acc = [x_acc; x];
   K_acc = [K_acc; K];
   P_acc = [P_acc; P];
   conf_acc = [conf_acc; 2*sqrt(conf)];
end
for epoch = 51:75
   [x, P, K, conf] = k_updatx(x, P, 1, b(epoch), 1, 0.5);
   x_acc = [x_acc; x];
   K_acc = [K_acc; K];
   P_acc = [P_acc; P];
   conf_acc = [conf_acc; 2*sqrt(conf)];
end
for epoch = 76:100
   [x, P, K, conf] = k_updatx(x, P, 1, b(epoch), 100, 0.5);
   x_acc = [x_acc; x];
   K_acc = [K_acc; K];
   P_acc = [P_acc; P];
   conf_acc = [conf_acc; 2*sqrt(conf)];
end
for i = 101:125
   [x, P, K, conf] = k_updatx(x, P, 1, b(i), 1, 0.5);
   x_acc = [x_acc; x];
   K_acc = [K_acc; K];
   P_acc = [P_acc; P];
   conf_acc = [conf_acc; 2*sqrt(conf)];
end

t = 1:125;
figure(1);
axis([0 126 -5 100])
set(gca,'ytick',[20 40 60 80])
hold on
plot(t ,x_acc, 'g-', 'Linewidth', 1)
plot(t, b, 'g.',t+1, x_acc, 'r', t+1, x_acc+conf_acc, 'b', ...
                                        t+1, x_acc-conf_acc, 'b')
hold off
print  k_simul1 -deps

figure(2);
subplot(2,1,1), plot(t, K_acc, 'r')
subplot(2,1,2), plot(t, P_acc, 'y')
print  k_simul2  -deps
%%%%%%%%%% end k_simul.m  %%%%%%%%%%%%%%%%%%%%%%
