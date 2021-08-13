%Example 3.2, SIAM Review paper
fprintf('running leaky driven cavity problem to produce Figure 3.4...\n\n')
close all
batchmode('NS3SR'),
f = figure(33);
set(f,'name','Stokes flow solution','numbertitle','off','integerhandle','off');
f = figure(66);
set(f,'name','Fully developed flow solution','numbertitle','off','integerhandle','off');
f = figure(67);
set(f,'name','Estimated error','numbertitle','off','integerhandle','off');

fprintf('\n\n')
reply = input('Press return to continue...','s');
sirev_unNS
f = figure(102);
set(f,'name','Flow snapshots','numbertitle','off','integerhandle','off');
f = figure(1);
set(f,'name','Time step evolution','numbertitle','off','integerhandle','off');


