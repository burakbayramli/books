% figformat % this is a user-defined m file that exists in my MATLAB path.  It's not necessary, but makes the plots pretty.
lowmsigma=1e9*[0.025,0.05,0.1,0.15,0.2];
lowmeta3=1e4*[18,6.3,3.1,2.6,1.2]*1e9;
highmsigma=1e9*[0.1,0.15,0.2,0.3,0.4,0.5];
highmeta3=[3.1e6,1.7e6,7.7e5,4.8e4,3e4,2.5e4]*1e9;

% now we generate the line fits (first order polynomials, using polyfit)
Plow=polyfit(lowmsigma,log(lowmsigma./lowmeta3),1);
Phigh=polyfit(highmsigma,log(highmsigma./highmeta3),1);
Lowfit=Plow(2)+lowmsigma*Plow(1);
Highfit=Phigh(2)+highmsigma*Phigh(1);

% now generate the plot, with label and legend
plot(lowmsigma,log(lowmsigma./lowmeta3),'+r',highmsigma,log(highmsigma./highmeta3),'bo',...
    lowmsigma,Lowfit,'-r',highmsigma,Highfit,'b-')
xlabel('\sigma (Pa)')
ylabel('ln(\sigma/\eta_{3}) (s^-1)')
legend('Low M','High M','location','best')

% save the plot as an .eps file
print(gcf, '../Figures/eyringfit.eps', '-depsc2')

%now get the activation voluems from the slope
vlow=2*Plow(1)*1.38e-23*300*1e30  % convert to cubic angstroms
vhigh=2*Phigh(1)*1.38e-23*300*1e30

