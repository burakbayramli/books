function vplstat(VPL,VPE,VAL1,VAL2,src_name)
%   VPE(Nx1) = upping error 
%   VPL(1xN) = VPL from integrity system (RAIM, SBAS, etc.)
%   VAL1 = Vertical alert limit 1
%   VAL2 = Vertical alert limit 2
%   src_name = label of vertical axis which will go as subscript after 'VPL' 
%
%*     Copyright c 1998 The board of trustees of the Leland Stanford     *
%*                      Junior University. All rights reserved.          *
%*     This script file may be distributed and used freely, provided     *
%*     this copyright notice is always kept with it.                     *
%*                                                                       *
%*     Questions and comments should be directed to Todd Walter at:      *
%*     walter@relgyro.stanford.edu                                       *
%VPLSTAT
%   VPLSTAT(FNAME, PATH, VAL1, VAL2) reads the given file of TMS Vertical 
%   Protection Limit statistics and plots the color coded histogram.  
%   In addition the regions of integrity failures and unavailability
%   are shown.  Multiple files can be combined by putting in vector of
%   strings for the FNAME (e. g. vplstat(['0421'; '0121'; '1621'])).
%   The optionalPATH is also a string and may also be in vector form 
%   (e. g. vplstat('0421',['.///////'; '../0624/']) strings must be
%   the same length). Default is './'.
%   The optional VAL1 and VAL2 arguments are given in meters (defaults
%   are 12m and 20m respectively).
%   See also : TMSSTAT, TRSNAMES, VERTSTAT, HPLSTAT
%   Example code for generating the file is included at the bottom of vplstat.m
%
%   NOTE: To plot on a non-color printer type:
% colormap('gray')
% % new colormap for colorbar (HPL and HPE plot)
% color_bar =[    
%     0.6980    0.6980    0.6980;
%     0.6905    0.6905    0.6905;
%     0.6830    0.6830    0.6830;
%     0.6754    0.6754    0.6754;
%     0.6679    0.6679    0.6679;
%     0.6604    0.6604    0.6604;
%     0.6528    0.6528    0.6528;
%     0.6453    0.6453    0.6453;
%     0.6378    0.6378    0.6378;
%     0.6302    0.6302    0.6302;
%     0.6227    0.6227    0.6227;
%     0.6152    0.6152    0.6152;
%     0.6076    0.6076    0.6076;
%     0.6001    0.6001    0.6001;
%     0.5926    0.5926    0.5926;
%     0.5850    0.5850    0.5850;
%     0.5775    0.5775    0.5775;
%     0.5700    0.5700    0.5700;
%     0.5624    0.5624    0.5624;
%     0.5549    0.5549    0.5549;
%     0.5474    0.5474    0.5474;
%     0.5398    0.5398    0.5398;
%     0.5323    0.5323    0.5323;
%     0.5247    0.5247    0.5247;
%     0.5172    0.5172    0.5172;
%     0.5097    0.5097    0.5097;
%     0.5021    0.5021    0.5021;
%     0.4946    0.4946    0.4946;
%     0.4871    0.4871    0.4871;
%     0.4795    0.4795    0.4795;
%     0.4720    0.4720    0.4720;
%     0.4645    0.4645    0.4645;
%     0.4569    0.4569    0.4569;
%     0.4494    0.4494    0.4494;
%     0.4419    0.4419    0.4419;
%     0.4343    0.4343    0.4343;
%     0.4268    0.4268    0.4268;
%     0.4193    0.4193    0.4193;
%     0.4117    0.4117    0.4117;
%     0.4042    0.4042    0.4042;
%     0.3967    0.3967    0.3967;
%     0.3891    0.3891    0.3891;
%     0.3816    0.3816    0.3816;
%     0.3741    0.3741    0.3741;
%     0.3665    0.3665    0.3665;
%     0.3590    0.3590    0.3590;
%     0.3515    0.3515    0.3515;
%     0.3439    0.3439    0.3439;
%     0.3364    0.3364    0.3364;
%     0.3289    0.3289    0.3289;
%     0.3213    0.3213    0.3213;
%     0.2966    0.2966    0.2966;
%     0.2719    0.2719    0.2719;
%     0.2472    0.2472    0.2472;
%     0.2225    0.2225    0.2225;
%     0.1977    0.1977    0.1977;
%     0.1730    0.1730    0.1730;
%     0.1483    0.1483    0.1483;
%     0.1236    0.1236    0.1236;
%     0.0989    0.0989    0.0989;
%     0.0742    0.0742    0.0742;
%     0.0494    0.0494    0.0494;
%     0.0247    0.0247    0.0247;
%          0         0         0];
% colormap(color_bar)

% other colors used in the plot
%color1 = [134/255 134/255 134/255]; - dark gray
color1 = [1 0.1 0.1];              % - red (original) 

%color2 = [174/255 174/255 174/255]; - middle gray
color2 = [1 0.55 0.55];            % - pink (original)

%color3 = [222/255 222/255 222/255]; - light gray
color3 = [1 1 0.5];                % - yellow (original)

%color4 = color2;                    - middle gray
color4 = [1 .55 0.3];              % - orange (original)

set(0,'DefaultTextFontName','Times');
set(0,'DefaultAxesFontName','Times');
set(0,'DefaultTextFontSize',18);
set(0,'DefaultAxesFontSize',18);

if nargin < 2
    error('Must input VPL and VPE!');
end
if nargin < 3
    VAL1 = 12;
end
if nargin < 4
    VAL2 = 20;
end

% size of VPL, which should be the same for VPE as well
n = size(VPL,2);

% VPE
j = floor(4.0*abs(VPE))+1;
j(find(j>=100)) = 100;
% VPL
k = floor(4.0*abs(VPL))+1;
k(find(k>=100)) = 100;

% initialize vpl_stat
data     = zeros(100,100);
diagonal = zeros(100,1);
for i = 1:n
    % statistics
    data(k(i),j(i)) = data(k(i),j(i))+1;
    % diagonal
    if((k(i) == j(i)) && (abs(VPE(i,1)) < abs(VPL(1,i))))
        diagonal(k(i),1) = diagonal(k(i),1)+1;
    end
end

c = 1;
colors = 'brygcmw';
clf

err_bin  = 0.125:0.25:24.875;
sig_bin  = 0.125:0.25:24.875;

seconds  = n;
sec_available = n;
diag_cnt = sum(diagonal);

% determine the number of points and axis ranges
n_pts = sum(sum(data));
if sec_available == 1
    epochs = seconds;
else
    epochs = n_pts;
end


d_err_bin = mean(diff(err_bin));
x_lo_bnd  = min(err_bin) - d_err_bin/2;
x_up_bnd  = max(err_bin) + d_err_bin/2;

d_sig_bin = mean(diff(sig_bin));
y_lo_bnd  = min(sig_bin) - d_sig_bin/2;
y_up_bnd  = max(sig_bin) + d_sig_bin/2;

z_lo_bnd  = 1;
z_up_bnd  = max(max(data));

% clear plot
clf;

% plot each data point as a pixel
[i,j]    = find(data);
face_mat = [[1 2 6 5]' [2 3 7 6]' [3 4 8 7]' ...
            [4 1 5 8]' [1 2 3 4]' [5 6 7 8]']';
colors   = colormap;
for idx = 1:length(i)
  z       = log10(data(i(idx),j(idx)));
  vtx_mat = [err_bin(j(idx)) + [-0.5 0.5 0.5 -0.5 -0.5 0.5 0.5 -0.5]'*d_err_bin ...
             sig_bin(i(idx)) + [-0.5 -0.5 0.5 0.5 -0.5 -0.5 0.5 0.5]'*d_sig_bin ...
             [0 0 0 0 z z z z]'];
  c_idx   = ceil(63*(log10(data(i(idx),j(idx)))/log10(z_up_bnd))) + 1;
  patch('Vertices',  vtx_mat, ...
        'Faces',     face_mat, ...
        'FaceColor', colors(c_idx,:), ...
        'EdgeColor', 'none');
end

% determine availability and # of integrity failures
i_diag1 = find(err_bin == VAL1 | err_bin == VAL2);
i_diag2 = find(err_bin < VAL1);
i_diag3 = find(err_bin > VAL2);
i_diag4 = find(err_bin > VAL1 & err_bin < VAL2);

i_fail1 = find((err_bin(j) >= VAL1 & sig_bin(i) < VAL1) |...
               (err_bin(j) >= VAL2 & sig_bin(i) < VAL2));
n_fail1 = sum(sum(diag(data(i(i_fail1),j(i_fail1)))))...
         - sum(diagonal(i_diag1));
i_fail2 = find(err_bin(j)./sig_bin(i) >=1.0 & err_bin(j) < VAL1);
n_fail2 = sum(sum(diag(data(i(i_fail2),j(i_fail2)))))...
         - sum(diagonal(i_diag2));
i_fail3 = find(err_bin(j)./sig_bin(i) >=1.0 & sig_bin(i) > VAL2);
n_fail3 = sum(sum(diag(data(i(i_fail3),j(i_fail3)))))...
         - sum(diagonal(i_diag3));
i_fail4 = find(err_bin(j)./sig_bin(i) >=1.0 & sig_bin(i) > VAL1...
                & err_bin(j) < VAL2);
n_fail4 = sum(sum(diag(data(i(i_fail4),j(i_fail4)))))...
         - sum(diagonal(i_diag4));
i_cont  = find(sig_bin(i) >= VAL2);
n_cont  = sum(sum(diag(data(i(i_cont),j(i_cont)))));
%i_avail = find(err_bin(j) < VAL2 & sig_bin(i) < VAL2);

i_avail1 = find(err_bin(j)./sig_bin(i) < 1.0 & sig_bin(i) < VAL1);
n_avail1 = sum(sum(diag(data(i(i_avail1),j(i_avail1)))))...
         + sum(diagonal(i_diag2));

i_avail2 = find(err_bin(j)./sig_bin(i) < 1.0 & sig_bin(i) < VAL2);
n_avail2 = sum(sum(diag(data(i(i_avail2),j(i_avail2))))) + sum(diagonal([i_diag2 i_diag4]'));

% set the axes limits and color values
set(gca,'XLim',[x_lo_bnd x_up_bnd]);
set(gca,'YLim',[y_lo_bnd y_up_bnd]);
set(gca,'CLim',[z_lo_bnd z_up_bnd]);

% show the region of IPV operation
HT = text(0.57*(VAL2 - x_lo_bnd) + x_lo_bnd, ...
     0.95*(VAL2 - y_lo_bnd) + y_lo_bnd, ...
     0.25*sqrt(z_up_bnd*z_lo_bnd), ...
     '{\fontsize{14pt}IPV Operation}');
pos = get(HT, 'Position');
pos = pos + [.05 -0.05 0];
set(HT, 'Position', pos);
set(HT, 'Color', [1 1 1]);
set(HT, 'FontSize', 14);

text(0.57*(VAL2 - x_lo_bnd) + x_lo_bnd, ...
     0.95*(VAL2 - y_lo_bnd) + y_lo_bnd, ...
     0.25*sqrt(z_up_bnd*z_lo_bnd), ...
     '{\fontsize{14pt}IPV Operation}');
if n_avail2/epochs >= 0.999995
  HT = text(0.53*(VAL2 - x_lo_bnd) + x_lo_bnd, ...
            0.89*(VAL2 - y_lo_bnd) + y_lo_bnd, ...
            0.25*sqrt(z_up_bnd*z_lo_bnd), ...
            '> 99.999%');
  pos = get(HT, 'Position');
  pos = pos + [.05 -0.05 0];
  set(HT, 'Position', pos);
  set(HT, 'Color', [1 1 1]);
  set(HT, 'FontSize', 14);
  HT = text(0.53*(VAL2 - x_lo_bnd) + x_lo_bnd, ...
            0.89*(VAL2 - y_lo_bnd) + y_lo_bnd, ...
            0.25*sqrt(z_up_bnd*z_lo_bnd), ...
            '> 99.999%');
  set(HT, 'FontSize', 14);
else
  HT = text(0.57*(VAL2 - x_lo_bnd) + x_lo_bnd, ...
            0.89*(VAL2 - y_lo_bnd) + y_lo_bnd, ...
            0.25*sqrt(z_up_bnd*z_lo_bnd), ...
            [num2str(100.0*n_avail2/epochs,'%6.3f'), '%']);
  pos = get(HT, 'Position');
  pos = pos + [.05 -0.05 0];
  set(HT, 'Position', pos);
  set(HT, 'Color', [1 1 1]);
  set(HT, 'FontSize', 14);
  HT = text(0.57*(VAL2 - x_lo_bnd) + x_lo_bnd, ...
            0.89*(VAL2 - y_lo_bnd) + y_lo_bnd, ...
            0.25*sqrt(z_up_bnd*z_lo_bnd), ...
            [num2str(100.0*n_avail2/epochs,'%6.3f'), '%']);
  set(HT, 'FontSize', 14);
end

% show the region of CAT I operation
HT = text(0.45*(VAL1 - x_lo_bnd) + x_lo_bnd, ...
     0.93*(VAL1 - y_lo_bnd) + y_lo_bnd, ...
     0.25*sqrt(z_up_bnd*z_lo_bnd), ...
     '{\fontsize{14pt}CAT I Oper.}');
pos = get(HT, 'Position');
pos = pos + [.05 -0.05 0];
set(HT, 'Position', pos);
set(HT, 'Color', [1 1 1]);
set(HT, 'FontSize', 14);

text(0.45*(VAL1 - x_lo_bnd) + x_lo_bnd, ...
     0.93*(VAL1 - y_lo_bnd) + y_lo_bnd, ...
     0.25*sqrt(z_up_bnd*z_lo_bnd), ...
     '{\fontsize{14pt}CAT I Oper.}');
if n_avail1/epochs >= 0.999995
  HT = text(0.4*(VAL1 - x_lo_bnd) + x_lo_bnd, ...
            0.84*(VAL1 - y_lo_bnd) + y_lo_bnd, ...
            0.25*sqrt(z_up_bnd*z_lo_bnd), ...
            '> 99.999%');
  pos = get(HT, 'Position');
  pos = pos + [.05 -0.05 0];
  set(HT, 'Position', pos);
  set(HT, 'Color', [1 1 1]);
  set(HT, 'FontSize', 14);
  HT = text(0.4*(VAL1 - x_lo_bnd) + x_lo_bnd, ...
            0.84*(VAL1 - y_lo_bnd) + y_lo_bnd, ...
            0.25*sqrt(z_up_bnd*z_lo_bnd), ...
            '> 99.999%');
  set(HT, 'FontSize', 14);
else
  HT = text(0.45*(VAL1 - x_lo_bnd) + x_lo_bnd, ...
            0.84*(VAL1 - y_lo_bnd) + y_lo_bnd, ...
            0.25*sqrt(z_up_bnd*z_lo_bnd), ...
            [num2str(100.0*n_avail1/epochs,'%6.3f'), '%']);
  pos = get(HT, 'Position');
  pos = pos + [.05 -0.05 0];
  set(HT, 'Position', pos);
  set(HT, 'Color', [1 1 1]);
  set(HT, 'FontSize', 14);
  HT = text(0.45*(VAL1 - x_lo_bnd) + x_lo_bnd, ...
            0.84*(VAL1 - y_lo_bnd) + y_lo_bnd, ...
            0.25*sqrt(z_up_bnd*z_lo_bnd), ...
            [num2str(100.0*n_avail1/epochs,'%6.3f'), '%']);
  set(HT, 'FontSize', 14);
end

% outline the region of integrity failures
patch([VAL1 VAL1 VAL2 VAL2 x_up_bnd x_up_bnd], ...
      [y_lo_bnd VAL1 VAL1 VAL2 VAL2 y_lo_bnd], ...
      -[0.5 0.5 0.5 0.5 0.5 0.5], ...
      color1);
HT = text(VAL2, ...
          0.5*(VAL2 - y_lo_bnd) + y_lo_bnd, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['HMI']);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');
pos = get(HT, 'Position');
pos = pos + [.05 -0.05 0];
set(HT, 'Position', pos);
set(HT, 'Color', color1);
HT = text(VAL2, ...
          0.5*(VAL2 - y_lo_bnd) + y_lo_bnd, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['HMI']);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');
HT = text(VAL2, ...
          0.4*(VAL2 - y_lo_bnd) + y_lo_bnd, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['epochs: ', int2str(n_fail1)]);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');
pos = get(HT, 'Position');
pos = pos + [.05 -0.05 0];
set(HT, 'Position', pos);
set(HT, 'Color', color1);
HT = text(VAL2, ...
          0.4*(VAL2 - y_lo_bnd) + y_lo_bnd, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['epochs: ', int2str(n_fail1)]);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');

% outline the lowest region of VPL failures
patch([x_lo_bnd VAL1 VAL1], ...
      [y_lo_bnd VAL1 y_lo_bnd], ...
      -[0.5 0.5 0.5], ...
      color2);
HT = text(0.67*(VAL1 - x_lo_bnd) + x_lo_bnd, ...
          0.35*(VAL1 - y_lo_bnd) + y_lo_bnd, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['MI']);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');
pos = get(HT, 'Position');
pos = pos + [.05 -0.05 0];
set(HT, 'Position', pos);
set(HT, 'Color', color2);
HT = text(0.67*(VAL1 - x_lo_bnd) + x_lo_bnd, ...
          0.35*(VAL1 - y_lo_bnd) + y_lo_bnd, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['MI']);
set(HT,'HorizontalAlignment','Center');
set(HT, 'FontSize', 14);
HT = text(0.67*(VAL1 - x_lo_bnd) + x_lo_bnd, ...
          0.25*(VAL1 - y_lo_bnd) + y_lo_bnd, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['epochs: ', int2str(n_fail2)]);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');
pos = get(HT, 'Position');
pos = pos + [.05 -0.05 0];
set(HT, 'Position', pos);
set(HT, 'Color', color2);
HT = text(0.67*(VAL1 - x_lo_bnd) + x_lo_bnd, ...
          0.25*(VAL1 - y_lo_bnd) + y_lo_bnd, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['epochs: ', int2str(n_fail2)]);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');

% outline the middle region of VPL failures
patch([VAL1 VAL2 VAL2], ...
      [VAL1 VAL2 VAL1], ...
      -[0.5 0.5 0.5], ...
      color2);
HT = text(0.67*(VAL2 - VAL1) + VAL1, ...
          0.39*(VAL2 - VAL1) + VAL1, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['MI']);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');
pos = get(HT, 'Position');
pos = pos + [.05 -0.05 0];
set(HT, 'Position', pos);
set(HT, 'Color', color2);
HT = text(0.67*(VAL2 - VAL1) + VAL1, ...
          0.39*(VAL2 - VAL1) + VAL1, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['MI']);
set(HT,'HorizontalAlignment','Center');
set(HT, 'FontSize', 14);
HT = text(0.67*(VAL2 - VAL1) + VAL1, ...
          0.25*(VAL2 - VAL1) + VAL1, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['epochs: ', int2str(n_fail4)]);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');
pos = get(HT, 'Position');
pos = pos + [.05 -0.05 0];
set(HT, 'Position', pos);
set(HT, 'Color', color2);
HT = text(0.67*(VAL2 - VAL1) + VAL1, ...
          0.25*(VAL2 - VAL1) + VAL1, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['epochs: ', int2str(n_fail4)]);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');

% outline the region of unavailability
patch([x_lo_bnd x_up_bnd x_up_bnd x_lo_bnd], ...
      [VAL2 VAL2 y_up_bnd y_up_bnd], ...
      -[0.5 0.5 0.5 0.5], ...
      color3);
HT = text(0.50*(x_up_bnd - x_lo_bnd) + x_lo_bnd, ...
          0.65*(y_up_bnd - VAL2) + VAL2, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          '{\fontsize{14pt}System Unavailable}');
set(HT,'HorizontalAlignment','Center');
pos = get(HT, 'Position');
pos = pos + [.05 -0.05 0];
set(HT, 'Position', pos);
set(HT, 'Color', color3);
HT = text(0.50*(x_up_bnd - x_lo_bnd) + x_lo_bnd, ...
          0.65*(y_up_bnd - VAL2) + VAL2, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          '{\fontsize{14pt}System Unavailable}');
set(HT,'HorizontalAlignment','Center');
HT = text(0.50*(x_up_bnd - x_lo_bnd) + x_lo_bnd, ...
          0.35*(y_up_bnd - VAL2) + VAL2, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['Alarm Epochs: ', int2str(n_cont)]);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');
pos = get(HT, 'Position');
pos = pos + [.05 -0.05 0];
set(HT, 'Position', pos);
set(HT, 'Color', color3);
HT = text(0.50*(x_up_bnd - x_lo_bnd) + x_lo_bnd, ...
          0.35*(y_up_bnd - VAL2) + VAL2, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['Alarm Epochs: ', int2str(n_cont)]);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');

% outline the region where integrity failures and unavailability overlap
patch([VAL2 x_up_bnd x_up_bnd], ...
      [VAL2 y_up_bnd VAL2], ...
      z_lo_bnd*-[0.45 0.45 0.45], ...
      color4);
HT = text(0.70*(x_up_bnd - VAL2) + VAL2, ...
          0.4*(y_up_bnd - VAL2) + VAL2, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['MI:']);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');
pos = get(HT, 'Position');
pos = pos + [.05 -0.05 0];
set(HT, 'Position', pos);
set(HT, 'Color', color4);
HT = text(0.70*(x_up_bnd - VAL2) + VAL2, ...
          0.4*(y_up_bnd - VAL2) + VAL2, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['MI:']);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');
HT = text(0.70*(x_up_bnd - VAL2) + VAL2, ...
          0.175*(y_up_bnd - VAL2) + VAL2, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          [int2str(n_fail3)]);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');
pos = get(HT, 'Position');
pos = pos + [.05 -0.05 0];
set(HT, 'Position', pos);
set(HT, 'Color', color4);
HT = text(0.70*(x_up_bnd - VAL2) + VAL2, ...
          0.175*(y_up_bnd - VAL2) + VAL2, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          [int2str(n_fail3)]);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');


hold on;
grid off;
% make the grid visible over the patch regions (integrity, unavailability)
ytick=get(gca,'YTick');
xtick=get(gca,'XTick');
nytick=length(ytick);
nxtick=length(xtick);
for i=1:nytick
    plot3([x_lo_bnd x_up_bnd], ytick(i)*[1 1], [0.6 0.6], 'k:');
end
for i=1:nxtick
    plot3(xtick(i)*[1 1], [y_lo_bnd y_up_bnd], [0.6 0.6], 'k:');
end

% label the axes and add a title
xlabel('{\fontsize{16pt}Error [m]}');
set(get(gca,'XLabel'),'Position', ...
    [ 0.50*(x_up_bnd - x_lo_bnd) + x_lo_bnd, ...
     -0.05*(y_up_bnd - y_lo_bnd) + y_lo_bnd z_lo_bnd]);
 
ylabel(['{\fontsize{16pt}VPL_{',src_name,'} [m]}']);
set(get(gca,'YLabel'),'Position', ...
    [-0.06*(x_up_bnd - x_lo_bnd) + x_lo_bnd, ...
      0.50*(y_up_bnd - y_lo_bnd) + y_lo_bnd z_lo_bnd]);

title (['Vertical Performance [',int2str(seconds),' seconds]']);
  
set(get(gca,'Title'),'FontSize',14);
set(get(gca,'Title'),'Position', ...
    [0.50*(x_up_bnd - x_lo_bnd) + x_lo_bnd, ...
     1.02*(y_up_bnd - y_lo_bnd) + y_lo_bnd z_lo_bnd]);

% put the sigma_v scale on the right hand y-axis
% K_v_pa = 5.33;
% for i = ceil(y_lo_bnd/K_v_pa):floor(y_up_bnd/K_v_pa)
%   if abs(i*K_v_pa - (0.5*(y_up_bnd - y_lo_bnd) + y_lo_bnd)) > 0.05*(y_up_bnd - y_lo_bnd)
%     plot3([0.99*(x_up_bnd - x_lo_bnd) + x_lo_bnd x_up_bnd], ...
%           i*K_v_pa*[1 1], ...
%           [0.65 0.65],'k');
%     text(1.02*(x_up_bnd - x_lo_bnd) + x_lo_bnd, i*K_v_pa, 0.65, int2str(i));
%   end
% end
% HT = text(1.03*(x_up_bnd - x_lo_bnd) + x_lo_bnd, ...
%           0.5*(y_up_bnd - y_lo_bnd) + y_lo_bnd, ...
%           0.65, ...
%           '{\fontsize{14pt}\sigma}_V (m)');
% set(HT,'HorizontalAlignment','Center');
% set(HT,'Rotation',90);

plot3([x_lo_bnd VAL1], [VAL1 VAL1], [-0.5 -0.5],'k');
% put in lines of constant prob
cont_lines=0;
if cont_lines
 plot3([0 y_up_bnd/K_v_pa], [0 y_up_bnd], [z_up_bnd z_up_bnd],'k');
 plot3([0 2*y_up_bnd/K_v_pa], [0 y_up_bnd], [z_up_bnd z_up_bnd],'k');
 plot3([0 3.29*y_up_bnd/K_v_pa], [0 y_up_bnd], [z_up_bnd z_up_bnd],'k');
 bnd_68 =err_bin(bound2(0.68,data')) + d_err_bin/2;
 bnd_95 =err_bin(bound2(0.95,data')) + d_err_bin/2;
 bnd_999=err_bin(bound2(0.999,data')) + d_err_bin/2;
 plot3(bnd_68, sig_bin + d_sig_bin/2, z_up_bnd*ones(100,1), 'k')
 plot3(bnd_95, sig_bin + d_sig_bin/2, z_up_bnd*ones(100,1), 'k')
 plot3(bnd_999, sig_bin + d_sig_bin/2, z_up_bnd*ones(100,1), 'k')
end 
%save data data sig_bin err_bin diagonal

err_bnd_68 =err_bin(bound2(0.68,sum(data)')) + d_err_bin/2;
err_bnd_95 =err_bin(bound2(0.95,sum(data)')) + d_err_bin/2;
err_bnd_999 =err_bin(bound2(0.999,sum(data)')) + d_err_bin/2;
patch([err_bnd_68 + 0.01*(x_up_bnd - x_lo_bnd) ...
       err_bnd_68 ...
       err_bnd_68 - 0.01*(x_up_bnd - x_lo_bnd)], ...
      [y_lo_bnd 0.02*(y_up_bnd - y_lo_bnd) y_lo_bnd], ...
      [z_up_bnd z_up_bnd z_up_bnd],'k');   
HT=text(err_bnd_68, 0.03*(y_up_bnd - y_lo_bnd), z_up_bnd, '68%');
set(HT,'Rotation',90);
patch([err_bnd_95 + 0.01*(x_up_bnd - x_lo_bnd) ...
       err_bnd_95 ...
       err_bnd_95 - 0.01*(x_up_bnd - x_lo_bnd)], ...
      [y_lo_bnd 0.02*(y_up_bnd - y_lo_bnd) y_lo_bnd], ...
      [z_up_bnd z_up_bnd z_up_bnd],'k');   
HT=text(err_bnd_95, 0.03*(y_up_bnd - y_lo_bnd), z_up_bnd, '95%');
set(HT,'Rotation',90);
patch([err_bnd_999 + 0.01*(x_up_bnd - x_lo_bnd) ...
       err_bnd_999 ...
       err_bnd_999 - 0.01*(x_up_bnd - x_lo_bnd)], ...
      [y_lo_bnd 0.02*(y_up_bnd - y_lo_bnd) y_lo_bnd], ...
      [z_up_bnd z_up_bnd z_up_bnd],'k');   
HT=text(err_bnd_999, 0.03*(y_up_bnd - y_lo_bnd), z_up_bnd,'99.9%');
set(HT,'Rotation',90);

sig_bnd_68 =sig_bin(bound2(0.68,sum(data')', epochs)) + d_sig_bin/2;
sig_bnd_95 =sig_bin(bound2(0.95,sum(data')', epochs)) + d_sig_bin/2;
sig_bnd_999 =sig_bin(bound2(0.999,sum(data')', epochs)) + d_sig_bin/2;
patch([x_up_bnd 0.98*(x_up_bnd - x_lo_bnd) + x_lo_bnd x_up_bnd], ...
          [sig_bnd_68 + 0.01*(y_up_bnd - y_lo_bnd) ...
           sig_bnd_68 ...
           sig_bnd_68 - 0.01*(y_up_bnd - y_lo_bnd)], ...
          [z_up_bnd z_up_bnd z_up_bnd],'k');   
text(0.925*(x_up_bnd - x_lo_bnd) + x_lo_bnd, sig_bnd_68, z_up_bnd, '68%');
patch([x_up_bnd 0.98*(x_up_bnd - x_lo_bnd) + x_lo_bnd x_up_bnd], ...
          [sig_bnd_95 + 0.01*(y_up_bnd - y_lo_bnd) ...
           sig_bnd_95 ...
           sig_bnd_95 - 0.01*(y_up_bnd - y_lo_bnd)], ...
          [z_up_bnd z_up_bnd z_up_bnd],'k');   
text(0.925*(x_up_bnd - x_lo_bnd) + x_lo_bnd, sig_bnd_95, z_up_bnd, '95%');
patch([x_up_bnd 0.98*(x_up_bnd - x_lo_bnd) + x_lo_bnd x_up_bnd], ...
          [sig_bnd_999 + 0.01*(y_up_bnd - y_lo_bnd) ...
           sig_bnd_999 ...
           sig_bnd_999 - 0.01*(y_up_bnd - y_lo_bnd)], ...
          [z_up_bnd z_up_bnd z_up_bnd],'k');   
text(0.9*(x_up_bnd - x_lo_bnd) + x_lo_bnd, sig_bnd_999, z_up_bnd, '99.9%');

% put the color scale up on the right hand side
H = colorbar('vert');
set(get(H,'Ylabel'),'String','{\fontsize{14pt}Number of Points per Pixel}');
set(H,'YScale','log');
set(H,'YLim',[z_lo_bnd z_up_bnd]);
set(H,'CLim', [z_lo_bnd z_up_bnd]);

if sec_available == 1
    display (['EGNOS diff for ', ...
              num2str(n_pts), ...
              ' out of ', ...
              num2str(seconds), ...
              ' seconds']);
end




set(H, 'linewidth',.8);
hold off
set(gca,'Fontsize',18);
%print -deps env_VPL