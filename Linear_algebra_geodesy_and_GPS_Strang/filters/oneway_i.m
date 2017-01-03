function oneway_i
%ONEWAY_I Evaluation of one-way data.
%         Estimation of ambiguities followed by an estimation
%	       of ionospheric delay
%         Finally we plot I for one-ways as measured at master
%         and rover receivers, plot of single differences and
%         plot of double differences

%Kai Borre 19-08-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 10-08-1097 $

%------------------
c0 = 299792458;
f1 = 154*10.23e6;
f2 = 120*10.23e6;
lambda1 = c0/f1;
lambda2 = c0/f2;
beta = (f1/f2)^2;
Big = 10^10;
Ionor = [];
Ionom = [];
e = exist('oneway_r.eps');
if e ~= 0, delete oneway_r.eps, end
pe = exist('oneway_m.eps');
if e ~= 0, delete oneway_m.eps, end
e = exist('oneway_s.eps');
if e ~= 0, delete oneway_s.eps, end
e = exist('oneway_d.eps');
if e ~= 0, delete oneway_d.eps, end
e = exist('oneway1.eps');
if e ~= 0, delete oneway1.eps, end
e = exist('oneway2.eps');
if e ~= 0, delete oneway2.eps, end
e = exist('oneway3.eps');
if e ~= 0, delete oneway3.eps, end
e = exist('oneway4.eps');
if e ~= 0, delete oneway4.eps, end
e = exist('oneway5.eps');
if e ~= 0, delete oneway5.eps, end
e = exist('oneway6.eps');
if e ~= 0, delete oneway6.eps, end

for rec = 1:2
   if rec == 1
      receiver = 'r';
   else
      receiver = 'm';
   end
   for sv = [2,9,16,23,26,27]
      datasv = ['one_' receiver int2str(sv)];
      filename = [datasv '.dat'];
      fid = fopen(filename);
      data = fread(fid,inf,'double');
      r = size(data,1);
      B = reshape(data,r/5,5);
      
      % Due to cold start we drop the first five epochs
      range = B(5:90,1:4);
      elevation = B(5:90,5);
      
      % Repair of clock reset; affects only pseudoranges
      spikes = diff(range(:,1));
      i = find(abs(spikes) > 280000);
      for j = 1:size(i)
         if spikes(i) < 0
            corr = 299792.458;
         else
            corr = -299792.458;
         end
         for k = i(j)+1:size(range,1)
            range(k,1) = range(k,1)+corr;
            range(k,3) = range(k,3)+corr;
         end
      end
      
      r = size(range,1);
      P_minus = eye(4)*Big;
      % Q = diag([Big Big 0 0]);  system covariance
      % This special choice of Q implies that invP_minus can
      % be updated the way it is coded below.
      % Our implementation avoids the numerical problems inherent
      % in the corresponding Kalman filter version.
      
      % Bayes sequential filter, cf.
      % Euler & Goad (1991) On Optimal Filtering of GPS Dual Frequency
      % 		   Observations Without Using Orbit Information
      % 		   Bulletin Geodesique, 65:130--143.
      A = [1	   1	         0	      0;
           1	  -1	   lambda1	      0;
           1	  beta	      0 	      0;
           1	 -beta	      0 	lambda2];
      x_plus = inv(A)*range(1,:)';  % Init. of filter using first obs.
      x = [];
      P = [];
      % P = P + Q; Kalman version uses this.
      % Bayes version uses inverse of information (or weight) matrix: invR
      % weight = [1/.3^2 1/0.003^2 1/.3^2 1/0.003^2];  % variance of obsv.
      % invR = diag(weight);
      
      for k = 1:r
         H = inv(P_minus(3:4,3:4));
         invP_minus = [zeros(2,4); zeros(2,2) H];
         x_minus = x_plus;
         % We make the variance on pseudoranges elevation dependent
         sigma = 0.08 + 4.5*exp(-elevation(k)/10);
         weight = [1/sigma^2 1/0.003^2 1/sigma^2 1/0.003^2];
         invR = diag(weight);
         ATR_inv = A'*invR;
         P_plus = inv(invP_minus + ATR_inv*A);
         K = P_plus*ATR_inv;
         x_plus = x_minus+K*(range(k,:)' - A*x_minus);
         P_minus = P_plus;
         x = [x x_plus];     % x = [rho*; I; N1; N2]
         P = [P P_plus];
      end
      %fprintf('\nEstimated ambiguity for N1-N2: %12.1f', x(3,r)-x(4,r))
      %fprintf('\nEstimated ambiguity for N1:    %12.1f\n', x(3,r))
      Ionosphere = (range(:,4)-lambda2*x(4,r)- ...
         (range(:,2)-lambda1*x(3,r)))/(1-(f1/f2)^2);
      
      if rec == 1
         Ionor = [Ionor Ionosphere];
      end
      if rec == 2
         Ionom = [Ionom Ionosphere];
      end
   end
end

fidf = figure;
fidp = plot(Ionor);
title('One-ways at rover','fontsize',16)
ylabel('Ionospheric delay  {\itI}  [m]','fontsize',16);
xlabel('Epochs, epoch interval  20 s','fontsize',16);
set(fidp,'Linewidth',1);
set(gca,'fontsize',16);
y = mean(Ionor)';
y = y + [-.2;-.3;.9;-.2;-.2;.2];
x = [80;80;80;80;80;80];
s = [' 2';' 9';'16';'23';'26';'27'];
text(x,y,s,'fontsize',16);
print oneway_r -deps

% test of white noise for PRN 2
autocorr(diff(Ionor(:,1)),1);
print oneway4 -deps

autocorr(Ionor(:,1),1);
print oneway1 -deps

figure;
fidq = plot(Ionom);
title('One-ways at master','fontsize',16)
ylabel('Ionospheric delay  {\itI}  [m]','fontsize',16);
xlabel('Epochs, epoch interval  20 s','fontsize',16);
set(fidq,'Linewidth',1);
set(gca,'fontsize',16);
y = mean(Ionom)';
y = y + [0.2;-0.6;1.0;-0.3;-0.2;0.3];
x = [80;80;80;80;80;80];
s = [' 2';' 9';'16';'23';'26';'27'];
text(x,y,s,'fontsize',16);
print oneway_m -deps

% plot of variogram function V(k)
%figure;
%varia = [];
%ttt = 0;
%for tt = [1,4,5]
   %  ttt = ttt+1;
%   varia = [varia variog(Ionom(:,tt))];
%end
%to_plot = round(.8*size(varia,1));
%q = varia(1:to_plot,:);
%plot(q)
%title('Variogram');

figure;
fids = plot(Ionor-Ionom);
title('Single differences','fontsize',16)
ylabel('Ionospheric delay  {\itI}  [m]','fontsize',16);
xlabel('Epochs, epoch interval  20 s','fontsize',16);
set(fids,'Linewidth',1);
set(gca,'fontsize',16);
y = mean(Ionor-Ionom)';
y = y + [-.03;-.01;.04;0;-.02;.02];
x = [80;80;80;80;80;80];
s = [' 2';' 9';'16';'23';'26';'27'];
text(x,y,s,'fontsize',16);
print oneway_s -deps

for i = [1,3]
   dcorr = autocorr(Ionor(:,i),0)+autocorr(Ionom(:,i),0)...
                        -crosscor(Ionor(:,i),Ionom(:,i))...
                          -crosscor(Ionom(:,i),Ionor(:,i));
   to_plot = round(.8*size(dcorr,1));
   q = dcorr(1:to_plot);
   figure
   hold on
   axis([-1 to_plot min(q) max(q)])
   bar(0:to_plot-1,q,.2)
   % axis([0 70 -6*1.e-4 6*1.e-4])
   ylabel('m^2','FontSize',14,'VerticalAlignment','baseline')
   set(gca,'FontSize',14)
   hold off
   if i == 1
      print oneway2 -deps
   end
   if i == 3
      print oneway5 -deps
   end
end

figure; % PRN 26 is selected as reference, hence a "5" in next line
M = Ionor-Ionor(:,[5 5 5 5 5 5])-Ionom+Ionom(:,[5 5 5 5 5 5]);
fidt = plot(M);
title('Double differences','fontsize',16)
ylabel('Ionospheric delay  {\itI}  [m]','fontsize',16);
xlabel('Epochs, epoch interval  20 s','fontsize',16);
set(fidt,'Linewidth',1);
set(gca,'fontsize',16);
y = mean(M)';
y = y + [-.01;-.02;.02;0;.02;.02];
x = [80;80;80;80;80;80];
s = [' 2';' 9';'16';'23';'26';'27'];
text(x,y,s,'fontsize',16);
print oneway_d -deps

for i = [1,3]
   %   DD = Ionor(:,i)-Ionor(:,5)-Ionom(:,i)+Ionom(:,5)
   ddcorr = autocorr(Ionor(:,i),0)+autocorr(Ionom(:,i),0)...
            +autocorr(Ionor(:,5),0)+autocorr(Ionom(:,5),0)...
             -crosscor(Ionor(:,i),Ionom(:,i))...
              -crosscor(Ionor(:,i),Ionor(:,5))...
                +crosscor(Ionor(:,i),Ionom(:,5))...	%
                -crosscor(Ionom(:,i),Ionor(:,i))...
                +crosscor(Ionom(:,i),Ionor(:,5))...
                -crosscor(Ionom(:,i),Ionom(:,5))...	%
                -crosscor(Ionor(:,5),Ionor(:,i))...
                +crosscor(Ionor(:,5),Ionom(:,i))...
                -crosscor(Ionor(:,5),Ionom(:,5))...	%
                +crosscor(Ionom(:,5),Ionor(:,i))...
                -crosscor(Ionom(:,5),Ionom(:,i))...
                -crosscor(Ionom(:,5),Ionor(:,5));
   to_plot = round(.8*size(ddcorr,1));
   q = ddcorr(1:to_plot);
   figure
   hold on
   bar(0:to_plot-1,q,.2)
   axis([-1 to_plot min(q) max(q)])
   %axis([0 70 -6*1.e-4 6*1.e-4])
   ylabel('m^2','FontSize',16,'VerticalAlignment','baseline')
   set(gca,'FontSize',16)
   hold off
   if i == 1
      print oneway3 -deps
   end
   if i == 3
      print oneway6 -deps
   end
end

Y = fft(q);
N = length(Y);
Y(1) = [];
power = abs(Y(1:N/2)).^2;
nyquist = 1/2;
freq = (1:N/2)/(N/2)*nyquist;
plot(freq,power), grid on
title('Periodogram')

%-----------------------------
function auto = autocorr(a,figuretrue);
%AUTOCORR Calculation of autocorrelation function
%	 for a given sequence of observations a
%   given as a column vector

%$Revision 1.0 $   $Date: 1997/08/17  $

m = mean(a);
[n,o] = size(a);
auto = zeros(n-1,1);

for shift = 0:n-2
   sum = 0;
   for i = 1:n-shift
      sum = sum+(a(i)-m)*(a(i+shift)-m);
   end;
   auto(shift+1,1) = sum/n; %%%%(n-shift-1);
end;

% The rightmost values of the autocorrelation function
% are not reliable; we omit the last 20%
to_plot = round(.8*size(auto,1));
q = auto(1:to_plot);

if figuretrue == 1
   figure
   hold on
   bar(0:to_plot-1,q,.2)
   axis([-1 to_plot min(q) max(q)])
   ylabel('m^2','FontSize',14,'VerticalAlignment','baseline')
   set(gca,'FontSize',14)
   hold off
end

%------------------------------------------
function cross = crosscor(a,b);
%CROSSCOR Calculation of crosscorrelation function
%	 betwen two given sequences of observations a and b
%   Each sequence must be given as a column vector

%$Revision 1.0 $  $Date: 1997/08/23  $

ma = mean(a);
mb = mean(b);
[n,o] = size(a);
cross = zeros(n-1,1);

for shift = 0:n-2
   sum = 0;
   for i = 1:n-shift
      sum = sum+(a(i)-ma)*(b(i+shift)-mb);
   end;
   cross(shift+1,1) = sum/n;  %%%(n-shift-1);
end;

%------------------------------------------
function vario = variog(a);
%VARIOG Calculation of variogram function for a set of
%	 observation series stored in the vector a

%$Revision 1.0 $   $Date: 1997/08/23  $

[n,o] = size(a);
auto = zeros(n-1,1);

for shift = 0:n-2
   sum = 0;
   for i = 1:n-shift
      sum = sum+(a(i)-a(i+shift))^2;
   end;
   vario(shift+1,1) = sum/n; %(n-shift-1);
end;
%%%%%%%%%%%%%% end oneway_i  %%%%%%%%%%%%%%%%%%%
