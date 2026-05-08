function f = freewall(f,areas,ci)
% applies the volumetric conservative freewall bc.

gamma = gather(f,areas,ci);
f = scatter(f,gamma,areas,ci);

