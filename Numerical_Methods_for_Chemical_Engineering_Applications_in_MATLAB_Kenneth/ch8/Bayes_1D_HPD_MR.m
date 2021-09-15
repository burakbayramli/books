% Bayes_1D_HPD_MR.m
%
% function [HPD_lo,HPD_hi] = Bayes_1D_HPD_MR( ...
%    bin_1Dc, bin_1Dp, j_plot_1D, alpha);
%
% This routine returns desired (1-alpha)
% Highest Probability Density (HPD) regions
% given the results of the 1-D marginal
% posterior density calculator,
% Bayes_MCMC_1Dmarginal_MR.m
%
% Routine inputs:
% bin_1Dc : the bin center positions of each marginal
%         density histogram
% bin_1Dp : the bin probability values of each
%         marginal density histogram
% j_plot_1D: the numbers of each parameter whose
%         marginal density has been calculated
% alpha : for 95% CI, use alpha = 0.05. The HPD is
%         returned that has (1-alpha) fraction of
%         total marginal density
%
% Routine outputs:
% HPD_lo : the minimum values in each HPD
% HPD_hi : the maximum values in each HPD
%
% K. J. Beers. MIT ChE. 12/15/2004. ver. 12/15/2004

function [HPD_lo,HPD_hi] = Bayes_1D_HPD_MR( ...
    bin_1Dc, bin_1Dp, j_plot_1D, alpha);

% extract dimensioning parameters
N_bins = size(bin_1Dc,2);
N_plots = size(bin_1Dc,1);
bin_delta = zeros(N_plots,1);
for m=1:N_plots
    bin_delta(m) = bin_1Dc(m,2) - bin_1Dc(m,1);
end

% initialize the output values
HPD_lo = zeros(N_plots,1);
HPD_hi = zeros(N_plots,1);

% for each parameter to be considered
for m = 1:N_plots
    j = j_plot_1D(m);  % get # of parameter
    % set vector of trial probability contour values
    p_c_max = max(bin_1Dp(m,:));
    p_c_vals = linspace(0,p_c_max,N_bins);
    alpha_vals = zeros(size(p_c_vals));
    % for each contour value, compute total density
    % within corresponding HPD
    for n=1:N_bins
        p_c = p_c_vals(n);
        k_above = find(bin_1Dp(m,:) >= p_c);
        alpha_vals(n) = bin_delta(m)*sum(bin_1Dp(m,k_above));
    end
    % make plot of alpha vs. p_c
    figure;
    plot(p_c_vals,alpha_vals);
    xlabel('p_c');
    ylabel('\alpha');
    title(['HPD contour levels for \theta(', ...
            int2str(j), ')']);
    % find p_c value for desired HPD
    for n=1:N_bins
        if(alpha_vals(n) <= (1-alpha))
            p_c_HPD = p_c_vals(n);
            break;
        end
    end
    % compute HPD for this contour value
    k_above = find(bin_1Dp(m,:) >= p_c_HPD);
    % make plot of HPD
    plot_HPD = zeros(1,N_bins);
    plot_HPD(k_above) = 1;
    figure;
    plot(bin_1Dc(m,:),plot_HPD);
    xlabel(['\theta(', int2str(j), ')']);
    ylabel('HPD region');
    title(['1-D HPD for \theta(', int2str(j), ...
            '), \alpha = ', num2str(alpha), ...
            ', p_c = ', num2str(p_c_HPD)]); 
    axis([min(bin_1Dc(m,:)) max(bin_1Dc(m,:))  ...
        -0.1   1.1]);
    % find lower and upper bounds of HPD
    HPD_lo(m) = min(bin_1Dc(m,k_above));
    HPD_hi(m) = max(bin_1Dc(m,k_above));
    
end


return;

