%Matlab code for Question 4 of Chapter 11

%Loop to do Monte Carlo and Importance sampling
%R is number of replications
R=10;
%Mean, scale and degrees of freedom for t importance function
ismean=3;
isscale=1;
isdof=2;
%Start all MC and Importance sampling sums at zero
thmeanmc=0;
th2momc=0;
thmeanis=0;
th2mois=0;
wsum=0;
wsum2=0;
for irep=1:R
    %Monte Carlo draw
    thetdraw=randn;    
    thmeanmc=thmeanmc+thetdraw;
    th2momc=th2momc + thetdraw^2;
    %Importance sampling draw. Note: tdis_rnd draws from Student-t
  thdrawis=ismean + sqrt(isscale)*tdis_rnd(1,isdof);
 
    %Importance sampling weight. Note: tdens evaluates the t density at a point
    w = norm_pdf(thdrawis,0,1)/tdens(thdrawis,ismean,isscale,isdof);
     thmeanis=thmeanis+w*thdrawis;
    th2mois=th2mois + w*thdrawis^2;
    wsum=wsum+w;
    wsum2=wsum2+w^2;
end
    
    thmeanmc=thmeanmc/R;
    th2momc=th2momc/R;
    thsdmc=sqrt(th2momc - thmeanmc^2);
    'Monte Carlo Posterior Mean and Standard Deviation'
    [thmeanmc thsdmc]
     thmeanis=thmeanis/wsum;
    th2mois=th2mois/wsum;
    thsdis=sqrt(th2mois - thmeanis^2);
    'Importance Sampling Posterior Mean and Standard Deviation'
    [thmeanis thsdis]
    'Mean and standard deviation of importance sampling weights'
    wmean=wsum/R;
    wstd=sqrt(wsum2/R - wmean^2);
    [wmean wstd]