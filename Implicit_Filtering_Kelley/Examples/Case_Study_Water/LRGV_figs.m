function LRGV_figs
% LRGV_figs
% Make the optimization landscapes in Chapter 10
%
%
% Put randstream at its initial settings.
%
s = RandStream.create('mt19937ar','seed',5489);
RandStream.setDefaultStream(s);
%defaultStream = RandStream.getDefaultStream;
%savedState=defaultStream.State;
%
%
% Labels for axes.
%
vlabs=cell(6,1);
vlabs{1}='Permanent Rights, {N}_R ';
vlabs{2}='Options, {N}_O ';
vlabs{3}='late alpha ';
vlabs{4}='late beta ';
vlabs{5}='early alpha ';
vlabs{6}='early beta ';

%Note: Each beta value must be >= to its respective alpha value


   ystart=[20000,0,1.1,1.3,.85,1.10]';
   bounds = [20000 40000; 0 10000; .7 2.2; .7 2.2; .7 2.2; .7 2.2];
%
% Model Parameters
%
NumberSims = 100;  % Number of realizations. Keep it above 100.
RELmin=0;           % Lower bound on reliability. 
                    % 0 makes the constraint inactive.
CVARmax=99;         % Upper bound on CVAR.
                    % 99 makes the constraint inactive.
iRo = 1500000;      % Initial reservoir level.
ifri = 0.3;         % Incoming water rights for month 1.
%
%
LRGV_str=struct('NumberSims',NumberSims,'RELmin',RELmin,...
        'CVARmax',CVARmax,'ifri',ifri,'iRo',iRo);
%
% Run with alphas and betas constant. Vary NR and NO.
%
nl=33; n1=1; n2=2;
xlab=vlabs{n1}; ylab=vlabs{n2};
%
% I will make arrays with several different values of RELmin
% CVARmax to illustrate how they affect the landscape. The basic structure
% will be the starting point.
%
RELminArray=[0, .995, 0, .995];
titles=cell(4,1);
titles{1}='100 Simulations';
titles{2}='Reliability={.995}';
titles{3}='CVARmax={1.1}';
titles{4}='Reliability={.995} and CVARmax={1.1}';
CVARmaxArray=[99, 99, 1.1, 1.1];
for i=1:1
figure(i)
%
% Put randstream at its initial settings.
% This will guarantee reproducible results if you run
% 'matlabpool close' before drawing the landscapes.
%
reset(RandStream.getDefaultStream);
%
LRGV_str.RELmin=RELminArray(i);
LRGV_str.CVARmax=CVARmaxArray(i);
zlist=plot_landscape(ystart, nl, n1, n2, xlab, ylab, bounds, ...
'LRGV_Parallel', LRGV_str);
axis([20000 40000 0 10000 6.0*1.d5 11.0*1.d5]);
title(titles{i});
end
