% Bayes_2D_HPD_MR.m
%
% function HPD_2D = Bayes_2D_HPD_MR( ...
%       bin_2Dic, bin_2Djc, bin_2Dp, ...
%       i_plot_2D, j_plot_2D, alpha);
%
% This routine returns desired (1-alpha)
% Highest Probability Density (HPD) regions
% given the results of the 2-D marginal
% posterior density calculator,
% Bayes_MCMC_2Dmarginal_MR.m
%
% Routine inputs:
% bin_2Dic = the center coordinates of each bin in the
%          i_plot histogram
% bin_2Djc = the center coordinates of each bin in the
%          j_plot histogram
% bin_2Dp = the marginal probability values for each bin,
%         where bin_p(m,n) contains the marginal density
%         at theta(i_plot) = bin_ic(m),
%         theta(j_plot_ = bin_jc(n).
% i_plot_2D = the label of the first parameter
% j_plot_2D = the label of the second parameter
% alpha : for 95% CI, use alpha = 0.05. The HPD is
%         returned that has (1-alpha) fraction of
%         total marginal density
% Routine outputs:
% HPD_2D = contains a 1 for each bin if it is in the HPD, else
%       it contains a zero
%
% K. J. Beers. MIT ChE. 12/15/2004. ver. 12/15/2004

function HPD_2D = Bayes_2D_HPD_MR(bin_2Dic, bin_2Djc, bin_2Dp, ...
    i_plot_2D, j_plot_2D, alpha);

% extract dimension parameters
N_bins = length(bin_2Dic);

% set area of each bin
bin_delta = zeros(2,1);
bin_delta(1) = bin_2Dic(2) - bin_2Dic(1);
bin_delta(2) = bin_2Djc(2) - bin_2Djc(1);
bin_area = bin_delta(1)*bin_delta(2);

% initialize HPD output structure
HPD_2D = zeros(N_bins,N_bins);

% set vector of trial probability contour values
p_c_max = max(max(bin_2Dp));
p_c_vals = linspace(0,p_c_max,N_bins);
alpha_vals = zeros(size(p_c_vals));

% for each contour value, compute total density
% within corresponding HPD
for n=1:N_bins
    p_c = p_c_vals(n);
    % compute net fraction of probability above this
    % contour value
    for ki=1:N_bins
        for kj=1:N_bins
            if(bin_2Dp(ki,kj) >= p_c)
                alpha_vals(n) = alpha_vals(n) + ...
                    bin_2Dp(ki,kj);
            end
        end
    end
    alpha_vals(n) = alpha_vals(n)*bin_area;
end
% make plot of alpha vs. p_c
figure;
plot(p_c_vals,alpha_vals);
xlabel('p_c');
ylabel('\alpha');
title(['2-D HPD contour levels for p(\theta(', ...
        int2str(i_plot_2D), '), \theta(', ...
        int2str(j_plot_2D), ') | y )']);
% find p_c value for desired HPD
p_c_HPD = 0;
for n=1:N_bins
    if(alpha_vals(n) <= (1-alpha))
        p_c_HPD = p_c_vals(n);
        break;
    end
end
% compute HPD for this contour value
for ki=1:N_bins
    for kj=1:N_bins
        if(bin_2Dp(ki,kj) >= p_c_HPD)
            HPD_2D(ki,kj) = 1;
        end
    end
end

% make contoura plot of HPD
figure;
contour(bin_2Dic,bin_2Djc,HPD_2D,1);
xlabel(['\theta(', int2str(i_plot_2D), ')']);
ylabel(['\theta(', int2str(j_plot_2D), ')']);
title(['2-D HPD for p(\theta(', ...
        int2str(i_plot_2D), '), \theta(', ...
        int2str(j_plot_2D), ') | y )']);
figure;
contourf(bin_2Dic,bin_2Djc,HPD_2D,1);
xlabel(['\theta(', int2str(i_plot_2D), ')']);
ylabel(['\theta(', int2str(j_plot_2D), ')']);
title(['2-D HPD for p(\theta(', ...
        int2str(i_plot_2D), '), \theta(', ...
        int2str(j_plot_2D), ') | y )']);

return;
