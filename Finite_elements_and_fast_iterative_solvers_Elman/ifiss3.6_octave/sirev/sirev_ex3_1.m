%Example 3.1, SIAM Review paper
fprintf('running uniform grid square domain problem to produce Figure 3.1(a)...\n\n')
close all
batchmode('P3'),
f = figure(10);
set(f,'name','Square domain uniform mesh','numbertitle','off','integerhandle','off');
f = figure(11);
set(f,'name','Square domain uniform mesh results','numbertitle','off','integerhandle','off');

fprintf('\n\n')
reply = input('Press return to continue...','s');

fprintf('running uniform grid L-shaped domain problem to produce Figure 3.1(b)...\n\n\n')
batchmode('P4'),
f = figure(10);
set(f,'name','L-shaped domain uniform mesh','numbertitle','off','integerhandle','off');
f = figure(11);
set(f,'name','L-shaped domain uniform mesh results','numbertitle','off','integerhandle','off');

fprintf('\n\n')
reply = input('Press return to continue...','s');
fprintf('running stretched grid L-shaped domain problem to produce Figure 3.3...\n\n\n')
batchmode('P4SR'),
f = figure(10);
set(f,'name','L-shaped domain stretched mesh','numbertitle','off','integerhandle','off');
f = figure(11);
set(f,'name','L-shaped domain stretched mesh results','numbertitle','off','integerhandle','off');
