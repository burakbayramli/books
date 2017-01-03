%SETUP run me at initialisation -- checks for a major indexing bug in matlab and initialises the paths
if exist('matlabpath')
    clear p s
    p(1).t(1)=1; % check if there is a bug in this matlab version
    s(1)=1;
    p(2).t=s(:);
    p(2).t; % gives the right answer
    s(1)=2; % should change s only
    if p(2).t~=1
        disp('Your matlab JIT compiler is buggy. It has now been turned off');
        eval('feature accel off')
    end
end
t={'','graphlayout','potentials','data','DemosExercises'};
p=pwd;
disp('Adding paths:')
for i=1:length(t)
    addpath([p,'/' t{i}])
    disp([p,'\' t{i}]);
end
if exist('../data'); addpath ../data; end