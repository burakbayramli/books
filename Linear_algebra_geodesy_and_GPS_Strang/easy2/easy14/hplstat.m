function hplstat(HPL,HPE,HAL,src_name)
% HPL(1xN) - Horizontal protection level  
% HPE(Nx1) - Horizontal position error = (easting.^2, northing.^2).^0.5
% HAL - Horizontal alert limit (define how large is the area for Normal operation 
%       [the line between white and yellow])
% src_name = label of vertical axis which will go as subscript after 'HPL'

%*     Copyright c 1998 The board of trustees of the Leland Stanford     *
%*                      Junior University. All rights reserved.          *
%*     This script file may be distributed and used freely, provided     *
%*     this copyright notice is always kept with it.                     *
%*                                                                       *
%*     Questions and comments should be directed to Todd Walter at:      *
%*     walter@relgyro.stanford.edu                                       *
%HPLSTAT
%   HPLSTAT(FNAME, HAL, PATH) reads the given file of TMS Horizontal 
%   Protection Limit statistics and plots the color coded histogram.  
%   In addition the regions of integrity failures and unavailability
%   are shown.  The optional HAL argument is given in meters (default 30m).
%   See also : TMSSTAT, TRSNAMES, VERTSTAT, VPLSTAT
%   Example code for generating the file is included at the bottom of hplstat.m
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
    error('Must input HPL and HPE!');
end
if nargin < 3
    HAL = 30;
end

% size of VPL, which should be the same for VPE as well
n = size(HPL,2);

% HPE
j = floor(2.0*abs(HPE))+1;
j(find(j>100)) = 100;
% HPL
k = floor(2.0*abs(HPL))+1;
k(find(k>100)) = 100;

% initialize hpl_stat
data     = zeros(100,100);
diagonal = zeros(100,1);
for i = 1:n
    % statistics
    data(k(i),j(i)) = data(k(i),j(i))+1;
    % diagonal
    if((k(i) == j(i)) && (abs(HPE(i,1)) < abs(HPL(1,i))))
        diagonal(k(i),1) = diagonal(k(i),1)+1;
    end
end

c = 1;
colors = 'brygcmw';
clf

err_bin  = 0.25:0.5:49.75;
sig_bin  = 0.25:0.5:49.75;

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
[i,j]=find(data);
face_mat=[[1 2 6 5]' [2 3 7 6]' [3 4 8 7]' ...
          [4 1 5 8]' [1 2 3 4]' [5 6 7 8]']';
colors=colormap;
for idx = 1:length(i)
  z = log10(data(i(idx),j(idx)));
  vtx_mat = [err_bin(j(idx))+[-0.5 0.5 0.5 -0.5 -0.5 0.5 0.5 -0.5]'*d_err_bin ...
             sig_bin(i(idx))+[-0.5 -0.5 0.5 0.5 -0.5 -0.5 0.5 0.5]'*d_sig_bin ...
             [0 0 0 0 z z z z]'];
  c_idx = ceil(63*(log10(data(i(idx),j(idx)))/log10(z_up_bnd))) + 1;
  patch('Vertices',  vtx_mat, ...
        'Faces',     face_mat, ...
        'FaceColor', colors(c_idx,:), ...
        'EdgeColor', 'none');
end

% determine availability and # of integrity failures
i_diag1 = find(err_bin == HAL);
i_diag2 = find(err_bin < HAL);
i_diag3 = find(err_bin > HAL);

i_fail1 = find(err_bin(j) >= HAL & sig_bin(i) < HAL);
n_fail1 = sum(sum(diag(data(i(i_fail1),j(i_fail1)))))...
         - sum(diagonal(i_diag1));
i_fail2 = find(err_bin(j)./sig_bin(i) >=1.0 & err_bin(j) < HAL);
n_fail2 = sum(sum(diag(data(i(i_fail2),j(i_fail2)))))...
         - sum(diagonal(i_diag2));
i_fail3 = find(err_bin(j)./sig_bin(i) >=1.0 & sig_bin(i) >= HAL);
n_fail3 = sum(sum(diag(data(i(i_fail3),j(i_fail3)))))...
         - sum(diagonal(i_diag3));
i_cont  = find(sig_bin(i) >= HAL);
n_cont  = sum(sum(diag(data(i(i_cont),j(i_cont)))));
%i_avail = find(err_bin(j) < HAL & sig_bin(i) < HAL);
i_avail = find(err_bin(j)./sig_bin(i) < 1.0 & sig_bin(i) < HAL);
n_avail = sum(sum(diag(data(i(i_avail),j(i_avail)))))...
         + sum(diagonal(i_diag2));


% set the axes limits and color values
set(gca,'XLim',[x_lo_bnd x_up_bnd]);
set(gca,'YLim',[y_lo_bnd y_up_bnd]);
set(gca,'CLim',[z_lo_bnd z_up_bnd]);


% show the region of normal operation
HT=text(0.37*(HAL - x_lo_bnd) + x_lo_bnd, ...
     0.93*(HAL - y_lo_bnd) + y_lo_bnd, ...
     0.25*sqrt(z_up_bnd*z_lo_bnd), ...
     '{\fontsize{14pt}Normal Operation}');
pos = get(HT, 'Position');
pos = pos + [.05 -0.05 0];
set(HT, 'Position', pos);
set(HT, 'Color', [1 1 1]); % white
text(0.37*(HAL - x_lo_bnd) + x_lo_bnd, ...
     0.93*(HAL - y_lo_bnd) + y_lo_bnd, ...
     0.25*sqrt(z_up_bnd*z_lo_bnd), ...
     '{\fontsize{14pt}Normal Operation}');
if n_avail/epochs >= .999995
  HT = text(0.33*(HAL - x_lo_bnd) + x_lo_bnd, ...
            0.86*(HAL - y_lo_bnd) + y_lo_bnd, ...
            0.25*sqrt(z_up_bnd*z_lo_bnd), ...
            '> 99.999%');
  pos = get(HT, 'Position');
  pos = pos + [.05 -0.05 0];
  set(HT, 'Position', pos);
  set(HT, 'Color', [1 1 1]); % white
  set(HT, 'FontSize', 14);
  HT = text(0.33*(HAL - x_lo_bnd) + x_lo_bnd, ...
            0.86*(HAL - y_lo_bnd) + y_lo_bnd, ...
            0.25*sqrt(z_up_bnd*z_lo_bnd), ...
            '> 99.999%');
  set(HT, 'FontSize', 14);
else
  HT = text(0.37*(HAL - x_lo_bnd) + x_lo_bnd, ...
            0.86*(HAL - y_lo_bnd) + y_lo_bnd, ...
            0.25*sqrt(z_up_bnd*z_lo_bnd), ...
            [num2str(100.0*n_avail/epochs,'%6.3f'), '%']);
  pos = get(HT, 'Position');
  pos = pos + [.05 -0.05 0];
  set(HT, 'Position', pos);
  set(HT, 'Color', [1 1 1]); % white
  set(HT, 'FontSize', 14);
  HT = text(0.37*(HAL - x_lo_bnd) + x_lo_bnd, ...
            0.86*(HAL - y_lo_bnd) + y_lo_bnd, ...
            0.25*sqrt(z_up_bnd*z_lo_bnd), ...
            [num2str(100.0*n_avail/epochs,'%6.3f'), '%']);
  set(HT, 'FontSize', 14);
end


% outline the region of integrity failures
patch([HAL HAL x_up_bnd x_up_bnd], ...
      [y_lo_bnd HAL HAL y_lo_bnd], ...
      -[0.5 0.5 0.5 0.5], ...
      color1); 
HT = text(0.50*(x_up_bnd - HAL) + HAL, ...
          0.55*(HAL - y_lo_bnd) + y_lo_bnd, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['HMI']);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');
pos = get(HT, 'Position');
pos = pos + [.05 -0.05 0];
set(HT, 'Position', pos);
set(HT, 'Color', color1); 
HT = text(0.50*(x_up_bnd - HAL) + HAL, ...
          0.55*(HAL - y_lo_bnd) + y_lo_bnd, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['HMI']);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');
HT = text(0.50*(x_up_bnd - HAL) + HAL, ...
          0.45*(HAL - y_lo_bnd) + y_lo_bnd, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['epochs: ', int2str(n_fail1)]);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');
pos = get(HT, 'Position');
pos = pos + [.05 -0.05 0];
set(HT, 'Position', pos);
set(HT, 'Color', color1);
HT = text(0.50*(x_up_bnd - HAL) + HAL, ...
          0.45*(HAL - y_lo_bnd) + y_lo_bnd, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['epochs: ', int2str(n_fail1)]);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');

% outline the region of HPL failures
patch([x_lo_bnd HAL HAL], ...
      [y_lo_bnd HAL y_lo_bnd], ...
      -[0.5 0.5 0.5], ...
      color2);  
HT = text(0.67*(HAL - x_lo_bnd) + x_lo_bnd, ...
          0.35*(HAL - y_lo_bnd) + y_lo_bnd, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['MI']);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');
pos = get(HT, 'Position');
pos = pos + [.05 -0.05 0];
set(HT, 'Position', pos);
set(HT, 'Color', color2); 
HT = text(0.67*(HAL - x_lo_bnd) + x_lo_bnd, ...
          0.35*(HAL - y_lo_bnd) + y_lo_bnd, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['MI']);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');
HT = text(0.67*(HAL - x_lo_bnd) + x_lo_bnd, ...
          0.25*(HAL - y_lo_bnd) + y_lo_bnd, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['epochs: ', int2str(n_fail2)]);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');
pos = get(HT, 'Position');
pos = pos + [.05 -0.05 0];
set(HT, 'Position', pos);
set(HT, 'Color', color2); 
HT = text(0.67*(HAL - x_lo_bnd) + x_lo_bnd, ...
          0.25*(HAL - y_lo_bnd) + y_lo_bnd, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['epochs: ', int2str(n_fail2)]);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');

% outline the region of unavailability
patch([x_lo_bnd x_up_bnd x_up_bnd x_lo_bnd], ...
      [HAL HAL y_up_bnd y_up_bnd], ...
      -[0.5 0.5 0.5 0.5], ...
      color3);          
HT = text(0.50*(x_up_bnd - x_lo_bnd) + x_lo_bnd, ...
          0.70*(y_up_bnd - HAL) + HAL, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          '{\fontsize{14pt}System Unavailable}');
set(HT,'HorizontalAlignment','Center');
pos = get(HT, 'Position');
pos = pos + [.05 -0.05 0];
set(HT, 'Position', pos);
set(HT, 'Color', color3); 
HT = text(0.50*(x_up_bnd - x_lo_bnd) + x_lo_bnd, ...
          0.70*(y_up_bnd - HAL) + HAL, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          '{\fontsize{14pt}System Unavailable}');
set(HT,'HorizontalAlignment','Center');
HT = text(0.50*(x_up_bnd - x_lo_bnd) + x_lo_bnd, ...
          0.55*(y_up_bnd - HAL) + HAL, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['Alarm Epochs: ', int2str(n_cont)]);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');
pos = get(HT, 'Position');
pos = pos + [.05 -0.05 0];
set(HT, 'Position', pos);
set(HT, 'Color', color3);
HT = text(0.50*(x_up_bnd - x_lo_bnd) + x_lo_bnd, ...
          0.55*(y_up_bnd - HAL) + HAL, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['Alarm Epochs: ', int2str(n_cont)]);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');

% outline the region where integrity failures and unavailability overlap
patch([HAL x_up_bnd x_up_bnd], ...
      [HAL y_up_bnd HAL], ...
      -z_lo_bnd*[0.45 0.45 0.45], ...
      color4);   
HT = text(0.70*(x_up_bnd - HAL) + HAL, ...
          0.325*(y_up_bnd - HAL) + HAL, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['MI']);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');
pos = get(HT, 'Position');
pos = pos + [.05 -0.05 0];
set(HT, 'Position', pos);
set(HT, 'Color', color4); 
HT = text(0.70*(x_up_bnd - HAL) + HAL, ...
          0.325*(y_up_bnd - HAL) + HAL, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['MI']);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');
HT = text(0.70*(x_up_bnd - HAL) + HAL, ...
          0.175*(y_up_bnd - HAL) + HAL, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['epochs: ', int2str(n_fail3)]);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');
pos = get(HT, 'Position');
pos = pos + [.05 -0.05 0];
set(HT, 'Position', pos);
set(HT, 'Color', color4); 
HT = text(0.70*(x_up_bnd - HAL) + HAL, ...
          0.175*(y_up_bnd - HAL) + HAL, ...
          0.25*sqrt(z_up_bnd*z_lo_bnd), ...
          ['epochs: ', int2str(n_fail3)]);
set(HT, 'FontSize', 14);
set(HT,'HorizontalAlignment','Center');

hold on;
grid off;
% make the grid visible over the patch regions (integrity, unavailability)
ytick  = get(gca,'YTick');
xtick  = get(gca,'XTick');
nytick = length(ytick);
nxtick = length(xtick);
for i = 1:nytick
    plot3([x_lo_bnd x_up_bnd], ytick(i)*[1 1], [0.6 0.6], 'k:');
end
for i = 1:nxtick
    plot3(xtick(i)*[1 1], [y_lo_bnd y_up_bnd], [0.6 0.6], 'k:');
end

% label the axes and add a title
xlabel('{\fontsize{16pt}Error [m]}');
set(get(gca,'XLabel'),'Position', ...
    [ 0.50*(x_up_bnd - x_lo_bnd) + x_lo_bnd, ...
     -0.05*(y_up_bnd - y_lo_bnd) + y_lo_bnd, ...
     z_lo_bnd]);
 
ylabel(['{\fontsize{16pt}HPL_{',src_name,'} [m]}']);
set(get(gca,'YLabel'),'Position', ...
    [-0.06*(x_up_bnd - x_lo_bnd) + x_lo_bnd, ...
      0.50*(y_up_bnd - y_lo_bnd) + y_lo_bnd z_lo_bnd]);
  
title (['Horizontal Performance [',int2str(seconds),' seconds]']);

set(get(gca,'Title'),'FontSize',14);
set(get(gca,'Title'),'Position', ...
    [0.50*(x_up_bnd - x_lo_bnd) + x_lo_bnd, ...
     1.02*(y_up_bnd - y_lo_bnd) + y_lo_bnd, ...
     z_lo_bnd]);


% put the sigma_H_major scale on the right hand y-axis
% K_h_pa = 6.18;
% for i = ceil(y_lo_bnd/K_h_pa):floor(y_up_bnd/K_h_pa)
%   if abs(i*K_h_pa - (0.5*(y_up_bnd - y_lo_bnd) + y_lo_bnd)) > ...
%      0.05*(y_up_bnd - y_lo_bnd)
%     plot3([.99*(x_up_bnd - x_lo_bnd) + x_lo_bnd x_up_bnd], ...
%           i*K_h_pa*[1 1], ...
%           [0.65 0.65], ...
%           'k');
%     text(1.02*(x_up_bnd - x_lo_bnd) + x_lo_bnd, i*K_h_pa, 0.65, int2str(i));
%   end
% end
% HT = text(1.04*(x_up_bnd - x_lo_bnd) + x_lo_bnd, ...
%           0.50*(y_up_bnd - y_lo_bnd) + y_lo_bnd, ...
%           0.65, ...
%           '{\fontsize{14pt}\sigma}_{H_{major}} (m)');
% set(HT,'HorizontalAlignment','Center');
% set(HT,'Rotation',90);

% put the color scale up on the right hand side
H = colorbar('vert');
set(get(H,'Ylabel'),'String','{\fontsize{14pt}Number of Points per Pixel}');
set(H,'YScale','log');
set(H,'YLim',[z_lo_bnd z_up_bnd]);
set(H,'CLim', [z_lo_bnd z_up_bnd]);

if sec_available == 1
    display (['EGNOS differential for ', ...
              num2str(n_pts), ...
              ' out of ', ...
              num2str(seconds), ...
              ' seconds']);
end

set(H, 'linewidth',.8);
hold off
set(gca,'Fontsize',18);
%print -deps env_HPL