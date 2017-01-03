function demoTcouple
% demoTcouple  Linear and quadratic fits to J-type thermocouple data
%
% Synopsis:  tcouple
%
% Input:     None
%
% Output:  Print fit coefficients and residuals.  Plot fit fcns and residuals

[v,t] = loadColData('Jtcouple.dat',2,1,3);   %  Read t = f(v) data from file

% --- Perform fits, evaluate fit function, compute residuals
vfit = linspace(min(v),max(v));
c1 = polyfit(v,t,1);  tfit1 = polyval(c1,vfit);   r1 = t - polyval(c1,v); 
c2 = polyfit(v,t,2);  tfit2 = polyval(c2,vfit);   r2 = t - polyval(c2,v);
c3 = polyfit(v,t,3);  tfit3 = polyval(c3,vfit);   r3 = t - polyval(c3,v);

fprintf('\nCurve fit coefficients\n              ');
fprintf('constant      emf           emf^2         emf^3\n');
fprintf('linear   ');  fprintf('  %12.5e',fliplr(c1));  fprintf('\n');
fprintf('quadratic');  fprintf('  %12.5e',fliplr(c2));  fprintf('\n');
fprintf('cubic    ');  fprintf('  %12.5e',fliplr(c3));  fprintf('\n');

% --- Plot fit and residuals
plot(v,t,'o',vfit,tfit1,'--',vfit,tfit2,'-',vfit,tfit3,'-.');
legend('Data','Linear','Quadratic','Cubic',2);   %  Legend in upper left corner
xlabel('emf (mV)');   ylabel('Temperature ({}^\circ C)');   grid on

f = figure;    %  new figure window for residuals
plot(v,r1,'o',v,r2,'s',v,r3,'d');  legend('Linear','Quadratic','Cubic',2);
xlabel('emf (mV)');   ylabel('Temperature residual    ({}^\circ C)');  grid on

fprintf('\nResiduals\n                ||r||_2      max error\n');
fprintf('linear         %8.5f      %8.5f\n',norm(r1),norm(r1,inf));
fprintf('quadratic      %8.5f      %8.5f\n',norm(r2),norm(r2,inf));
fprintf('cubic          %8.5f      %8.5f\n',norm(r3),norm(r3,inf));
