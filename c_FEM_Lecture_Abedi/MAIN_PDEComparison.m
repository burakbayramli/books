function MAIN_PDEComparison(configName, resMin, resMax)
fs = 22;
if (nargin < 1)
    configName = 'config_DG_HalfSine.txt';
    % ellipic
    configName = 'config_DG_Sine.txt';
    % ellipic, parabolic, hyperbolic
    configName = 'config_DG_LeftEssential0_RightNatural1.txt';
    % parabolic, hyperbolic
    configName = 'config_DG_PeriodicSine.txt';
    % hyperbolic
    configName = 'config_DG_Hyperbolic2regions.txt';
end

if (nargin < 2)
    resMin = 8;
end

if (nargin < 3)
    resMax = 1;
end

fid = fopen(configName, 'r');
pdeFEMtmp = PDE1DFEM;
pdeFEMtmp = pdeFEMtmp.read(fid);
PDEtype = pdeFEMtmp.PDEtype;
fclose(fid);
numConconvergentError = 0;
if (PDEtype ~= 2) % elliptic
    %% PARAMETERs ORDER: epsilon / sigma
    parasNames{1} = 'epsilon';
    parasNamesLatex{1} = '\epsilon';
    parasNames{2} = 'sigma';
    parasNamesLatex{2} = '\sigma';
    
    if (PDEtype == 0)
        % eps
        paras1 = [1, -1, 1, -1, 0, 0];
        % sigmas
        paras2 = [1, 1, 0, 0, 1, 0];        
        % the last case does not converge, so it's excluded from plots by
        % using the number below
        numConconvergentError = 4;
        if (strcmp(configName, 'config_DG_LeftEssential0_RightNatural1.txt') == 1)
            numConconvergentError = 2;
        end
    elseif (PDEtype == 1)
        % eps
        paras1 = [1, 0, 0, -1, -1];
        % sigmas
        paras2 = [0, 0, 1, 1, 0];
        % the last case does not converge, so it's excluded from plots by
        % using the number below
        numConconvergentError = 1;
        if (strcmp(configName, 'config_DG_LeftEssential0_RightNatural1.txt') == 1)
            numConconvergentError = 2;
            resMin = 7;
        end
    end
elseif (PDEtype == 2)
    %% PARAMETERs ORDER: star option (1->R, 0->1)  / lambdaScalingOn (0->off, 1->on)
    parasNames{1} = 'StarR';
    parasNamesLatex{1} = '*R:';
    parasNames{2} = 'DimConsistency';
    parasNamesLatex{2} = '\lambda';

    %% PARAMETERs ORDER: star option (1->R, 0->1)  / lambdaScalingOn (0->off, 1->on)
    paras1 = [1, 0, 1];
    parasNames{1} = 'StarR';
    parasNamesLatex{1} = '*R:';
    paras2 = [1, 1, 0];
    parasNames{2} = 'DimConsistency';
    parasNamesLatex{2} = '\lambda';

    % Riemann star on
    paras1 = [0, 1, 0, 1];
    % lambda scaling on
    paras2 = [1, 1, 0, 0];
    numConconvergentError = 1;
end

 % elliptic / parabolic example in class (length = 3, T(0) = 0, q(3) = 1)
if 0 % if (strcmp(configName, 'config_DG_Sine.txt') == 1)
    % eps
    paras1 = [1, -1, 1, 0];
    % sigmas
    paras2 = [1, 1, 0, 1];
    numConconvergentError = 0;
 end
 % elliptic / parabolic example 
 % T_(exact) = sin(x * pi / 2)
 if (strcmp(configName, 'config_DG_HalfSine.txt') == 1)
    % eps
    paras1 = [1, -1, 0];
    % sigmas
    paras2 = [0, 0, 100];
    numConconvergentError = 0;
    
 end
 if (strcmp(configName, 'config_DG_PeriodicSine.txt') == 1)
        resMax = 2;
 end
 
% periodic domain [0, 2pi] IC: T = sin(x) (TDot(x) = 0 for hyperbolic case)
% this example is used for Elliptic, Parabolic, and Hyperbolic cases

cs = length(paras1);
numModes = cs;
numModesErrorAnalysis = numModes - numConconvergentError;

for mode = 1:numModes
    namesMode{mode} = [parasNamesLatex{1}, ' = ', num2str(paras1( mode)), ', ', parasNamesLatex{2}, ' = ', num2str(paras2( mode))];
    namesModeFile{mode} = [parasNames{1}, '=', num2str(paras1( mode)), ', ', parasNames{2}, '=', num2str(paras2( mode))];
end

for mode = 1:numModesErrorAnalysis
    namesModeConvergent{mode} = namesMode{mode};
end

ress = resMax:resMin;
numRes = length(ress);
for ri = 1:numRes
    namesRes{ri} = ['L/h = ', num2str(ress(ri))];
    namesResFile{ri} = ['InverseOfRes', num2str(ress(ri))];
    numERes(ri) = power(2, ress(ri));
end

for mode = 1:numModes
    % (eps, sigma) for elliptic / parabolic
    % (Riemann on, lambda scaling on) for hyperbolic
    extraPara(1) = paras1(mode);
    extraPara(2) = paras2(mode);

    figure(mode);
    clf;
    for ri = 1:numRes
        res = ress(ri);
        numElements = numERes(ri);
        % slnDGKappaYxs are DG solutions for kappa u,x (e.g. heat flux,
        % stress, etc)
        [pdeFEM, slnDGXs{mode}{ri}, slnDGYs{mode}{ri}, slnXs{mode}{ri}, slnYs{mode}{ri}, slnScalarData{1}(mode,ri), slnScalarData{2}(mode, ri), slnDGKappaYxs{mode}{ri}] = Solve1D_PDE(configName, numElements, extraPara);
        timeComparison = 0;
        plot(slnDGXs{mode}{ri}, slnDGYs{mode}{ri}, 'LineWidth', 2);
        slnDGXsExact = slnDGXs{mode}{ri};
        if (pdeFEM.PDEtype ~= 0)
            timeComparison = pdeFEM.finalTime;
        end
        [slnDGYsExact, slnDGDotYsExact, hasExact, slnDGKappaYxsExact, hasExactYxsKappa] =  pdeFEM.PDEExactSolution(slnDGXsExact, timeComparison);
        if ((hasExact) && (mode <= numModesErrorAnalysis))
            error(mode,ri) = l2ErrorDG(slnDGXsExact, slnDGYsExact, slnDGYs{mode}{ri});
            slnScalarData{3}(mode, ri) = log2(error(mode, ri));
        else
            slnScalarData{3}(mode, ri) = NaN;
        end
        hold on;
    end
    namesLegend = namesRes;
    if (hasExact)
        namesLegend{length(namesLegend) + 1} = 'exact';
        plot(slnDGXsExact, slnDGYsExact, 'Color', 'k', 'LineWidth', 2);
        hold on;
    end
    legend(namesLegend, 'FontSize', fs);
    legend('boxoff');
    xlabel('X', 'FontSize', fs);
    ylabel('u', 'FontSize', fs);
    title(namesMode{mode}, 'FontSize', fs);
    print('-dpng', [namesModeFile{mode}, '.png']);
    hold off;
    
    figure(10 + mode);
    clf;
    for ri = 1:numRes
        res = ress(ri);
        numElements = numERes(ri);
        timeComparison = 0;
        plot(slnDGXs{mode}{ri}, slnDGKappaYxs{mode}{ri}, 'LineWidth', 2);
        hold on;
        slnDGXsExact = slnDGXs{mode}{ri};
        if (pdeFEM.PDEtype ~= 0)
            timeComparison = pdeFEM.finalTime;
        end
    end
    namesLegend = namesRes;
    if (hasExactYxsKappa)
        namesLegend{length(namesLegend) + 1} = 'exact';
        plot(slnDGXsExact, slnDGKappaYxsExact, 'Color', 'k', 'LineWidth', 2);
        hold on;
    end
    legend(namesLegend, 'FontSize', fs);
    legend('boxoff');
    xlabel('X', 'FontSize', fs);
    ylabel('\kappa u_x', 'FontSize', fs);
    title(namesMode{mode}, 'FontSize', fs);
    print('-dpng', [namesModeFile{mode}, '_der.png']);
    hold off;
end

close('all');


for ri = 1:numRes
    res = ress(ri);
    figure(ri);
    clf;
    for mode = 1:numModesErrorAnalysis
            plot(slnDGXs{mode}{ri}, slnDGYs{mode}{ri}, 'LineWidth', 2);
        hold on;
    end
    namesLegend = namesModeConvergent;
    if (hasExact)
        namesLegend{length(namesLegend) + 1} = 'exact';
        plot(slnDGXsExact, slnDGYsExact, 'Color', 'k', 'LineWidth', 2);
        hold on;
    end
    legend(namesLegend, 'FontSize', fs);
    legend('boxoff');
    xlabel('X', 'FontSize', fs);
    ylabel('u', 'FontSize', fs);
    title(['res = ', num2str(res)], 'FontSize', fs);
    print('-dpng', [namesResFile{ri}, '.png']);
    hold off;
    
    figure(10 + ri);
    clf;
    for mode = 1:numModesErrorAnalysis
            plot(slnDGXs{mode}{ri}, slnDGKappaYxs{mode}{ri}, 'LineWidth', 2);
        hold on;
    end
    namesLegend = namesModeConvergent;
    if (hasExactYxsKappa)
        namesLegend{length(namesLegend) + 1} = 'exact';
        plot(slnDGXsExact, slnDGKappaYxsExact, 'Color', 'k', 'LineWidth', 2);
        hold on;
    end
    legend(namesLegend, 'FontSize', fs);
    legend('boxoff');
    xlabel('X', 'FontSize', fs);
    ylabel('\kappa u_x', 'FontSize', fs);
    title(['res = ', num2str(res)], 'FontSize', fs);
    print('-dpng', [namesResFile{ri}, '_der.png']);
    hold off;
end
close('all');

scalarNames = {'min(Re(\lambda))', 'max(Re(\lambda))', 'log_2(L_2(T-T^h))'};
scalarNames4File = {'min_Re_lambda', 'max_Re_lambda', 'log2_L2_error'};
for si = 1:length(scalarNames)
    txt = scalarNames(si);
    plot(ress, slnScalarData{si}, 'LineWidth', 2);
    xlabel('log_2(L/h)', 'FontSize', fs);
    ylabel(txt, 'FontSize', fs);
    legend(namesModeConvergent, 'FontSize', fs);
    legend('boxoff');
    title(txt, 'FontSize', fs);
    print('-dpng', [scalarNames4File{si}, '.png']);
end

close('all');