function [pdeFEM, slnDGXs, slnDGYs, slnXs, slnYs, lambdaMin, lambdaMax, slnDGKappaYxs] = Solve1D_PDE(configName, numElementNew, extraPara)
if (nargin < 1)
    configName = 'config_DG_PeriodicSine.txt';
    configName = 'config_DG_LeftEssential0_RightNatural1.txt';
    configName = 'config_DG_Sine.txt';
end
if (nargin < 2)
    numElementNew = -1;
end
if (nargin < 3)
    extraPara = [];
end
fid = fopen(configName, 'r');
pdeFEM = PDE1DFEM;
pdeFEM = pdeFEM.read(fid, extraPara);
fclose(fid);
pdeFEM = pdeFEM.setNumElements(numElementNew);
[pdeFEM, F, A, slnDGXs, slnDGYs, slnXs, slnYs, slnDGKappaYxs] = pdeFEM.ComputeDG_Sln();
lambdaMin = pdeFEM.KminDReal;
lambdaMax = pdeFEM.KmaxDReal;