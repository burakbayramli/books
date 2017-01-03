% demoLinterp  Script demonstrating linterp function with thermocouple data
 
[v,t] = loadColData('Jtcouple.dat',2,1,3);

linterp(emf,t,0.8)   %  interpolate dt = fcn(emf) at emf = 0.8

linterp(t,emf,37)    %  interpolate emf = fcn(dt) at dt = 37
