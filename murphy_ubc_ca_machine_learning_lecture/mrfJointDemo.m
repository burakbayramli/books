% mrfJointDemo.m
%
% Constructor is pot = tabularPot(domain, sizes, T)
% domain = variables in the potential
% sizes = number of states for each variable
% T = table of numbers
f1 = tabularPot([1 2 3], [2 2 2], [1:8]);
f2 = tabularPot([2 4], [2 2], [0.5 1 1.5 2]);

% Combine potentials
J = tabularPot(1:4, [2 2 2 2]);
J = multiplyByPot(f, f1);
J = multiplyByPot(f, f2);

% Or more directly
J = multiplyPots(f1, f2)

% Convert object to array
unnormalizedJoint = J.T(:)'

% Normalize the array
[joint, Z] = normalize(unnormalizedJoint) % Z=94
bar(joint)

% alternative method of computing Z
m = marginalizePot(f, []);
Z = m.T; % m.T = 58

