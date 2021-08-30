clear all
close all
figformat
lnpE =[0.0291667 0.0680556	0.090625 0.0982639	0.102778 0.112847 0.120139	0.123611	...
    0.127778	0.128	0.134722 0.136806	0.139236	0.144444	0.145	...	
0.151042	0.152778	0.156597	0.163542	0.164	0.168056	0.173611	0.178819	...
0.180903	0.182292	0.185417	0.186	0.188542	0.194444	0.195486	0.203125	...
0.209722	0.211458];

lnlninverseps=[-3.53698 -2.8328	-2.40836  -2.10932 -1.86817	-1.66559 -1.49196 -1.36656	-1.22186 ...	
-1.08682 -0.961415 -0.874598 -0.7781350 -0.681672 -0.585209 -0.498392 -0.401929 -0.334405 -0.237942	...
-0.151125 -0.0257235 0.0803859 0.14791 0.234727 0.321543 0.379421 0.485531 0.562701 0.649518 ...
0.77492 0.890675 1.06431 1.26688];	

pE=exp(lnpE);
lninverseps=exp(lnlninverseps);
inverseps=exp(lninverseps);
ps=1./inverseps;

plot(pE,ps,'b+')
xlabel('p/E')
ylabel('p_{s}')
print(gcf,'../figures/weibull_psvsp','-depsc2')

figure
plot(pE,inverseps,'b+')
xlabel('p/E')
ylabel('1/p_{s}')
print(gcf,'../figures/weibull_invpsvsp','-depsc2')

figure
plot(lnpE,lninverseps,'b+')
xlabel('ln(p/E)')
ylabel('ln(1/p_{s})')
print(gcf,'../figures/weibull_lninvpsvslnp','-depsc2')

figure
plot(lnpE,lnlninverseps,'b+')
hold on
xlabel('ln(p/E)')
ylabel('ln(ln(1/p_{s}))')
linearfit=fit(transpose(lnpE),transpose(lnlninverseps),'linear');
plot(lnpE,linearfit,'r-')
print(gcf,'../figures/weibull_lnlninvpsvslnp','-depsc2')



