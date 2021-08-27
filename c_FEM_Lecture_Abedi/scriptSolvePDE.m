configNameWOExt = 'config_DG_Sine';
configNameWOExt = 'config_DG_Hyperbolic2regions';
configNameWOExt = 'config_DG_PeriodicSine'; 
configNameWOExt = 'config_DG_LeftEssential0_RightNatural1'; 

configName = [configNameWOExt, '.txt'];
close('all');
fid = fopen(configName, 'r');
pdeFEM = PDE1DFEM;
extraPara = [];
numElementNew = -1; % not overwriting files number of elements
pdeFEM = pdeFEM.read(fid, extraPara);
fclose(fid);
pdeFEM = pdeFEM.setNumElements(numElementNew);
%pdeFEM.Test2MaterialSolution();
[pdeFEM, slnDGXs, slnDGYs, slnXs, slnYs, lambdaMin, lambdaMax, slnDGKappaYxs] = Solve1D_PDE(configName);
%pdeFEM.K
%pdeFEM.M
fileName = [configNameWOExt, '.out'];
fido = fopen(fileName, 'w');
fprintf(fido, 'lambdaMin\t');
fprintf(fido, '%g\t', lambdaMin);
fprintf(fido, '\tlambdaMax\t');
fprintf(fido, '%g\n', lambdaMax);

fprintf(fido, '\nA\n');
fprintf(fido, '%g\n', pdeFEM.As{length(pdeFEM.As)});

fprintf(fido, '\nslnDGXs\n');
fprintf(fido, '%g\n', slnDGXs);
fprintf(fido, '\n\nslnDGYs\n');
fprintf(fido, '%g\n', slnDGYs);
fprintf(fido, '\n\nslnXs\n');
fprintf(fido, '%g\n', slnXs);
fprintf(fido, '\n\nslnYs\n');
fprintf(fido, '%g\n', slnYs);
fprintf(fido, '\n\nslnDGKappaYxs\n');
fprintf(fido, '%g\n', slnDGKappaYxs);
fprintf(fido, '\n\nK\n');
ndof = pdeFEM.ndof;
for i = 1:ndof
    for j = 1:ndof
        fprintf(fido, '%g\t', pdeFEM.K(i, j));
    end
    fprintf(fido, '%\n');
end
    
if (pdeFEM.PDEtype ~= 0)
    fprintf(fido, '\n\nM\n');
    for i = 1:ndof
        for j = 1:ndof
            fprintf(fido, '%g\t', pdeFEM.M(i, j));
        end
        fprintf(fido, '%\n');
    end
    if (pdeFEM.PDEtype == 2)
        fprintf(fido, '\n\nC\n');
        for i = 1:ndof
            for j = 1:ndof
                fprintf(fido, '%g\t', pdeFEM.CMat(i, j));
            end
            fprintf(fido, '%\n');
        end
    end
end
fclose(fido);

figure(1);
plot(slnDGXs, slnDGYs);
fileName = [configNameWOExt, '_u.png'];
print('-dpng', fileName);
figure(2);
plot(slnDGXs, slnDGKappaYxs);
fileName = [configNameWOExt, '_DuDx.png'];
print('-dpng', fileName);