# Power analysis for BACI design with a single control and single impact site, but 
# with multiple years before/after impact
# 2014-11-26 CJS ggplot, split, ##***

# Example of of the fish analysis.
# Fish counts are quite variable and a BACI difference of 10 is important
#    We use mu_TA=20, mu_TB=30, mu_CB=30, mu_cA=30
#
# We have the year-to-year variation (sdYear) is 31.71
# The site-to-site variation can be set to any arbitrary value (e.g. 0)
# The residual and site-year variance components are confounded to set
# the sdYearSite to 27.07 and the sdResidual to 0
# Also set the n_TA=1 etc.
#
# We examine various scenarios for the number of sites and number subsamples 

# Get the BACI power program
source("../baci-power.r")



# Illustrate how to get computations for a single scenario
baci.power(n_TA=1, n_TB=1, n_CA=1, n_CB=1, 
            ns_T=1, ns_C=1, 
            ny_B=12, ny_A=13, 
            mu_TA=20, mu_TB=30, mu_CA=30, mu_CB=30, 
            sdYear=31.71, sdSite=99, sdSiteYear=27.07, sdResid=0)



##***part500b;
results <- NULL
# Note that because there is only 1 site, there is NO variance component
# associated with site. Use any value here. In general, the site variance
# component is not important because when you meaure the same sites over time
# the site effects cancel out (like blocking) and so site-to-site variability
# is not important. The site-year variation really is what drives the BACI power.
results <- rbind(results,
            baci.power(n_TA=1, n_TB=1, n_CA=1, n_CB=1, 
            ns_T=1, ns_C=1, 
            ny_B=12, ny_A=13, 
            mu_TA=20, mu_TB=30, mu_CA=30, mu_CB=30, 
            sdYear=31.71, sdSite=0, sdSiteYear=27.07, sdResid=0))


##***part500e;
# More scenarios are compared            

results <- rbind(results,
            baci.power(n_TA=1, n_TB=1, n_CA=1, n_CB=1, 
            ns_T=1, ns_C=1, 
            ny_B=24, ny_A=26, 
            mu_TA=20, mu_TB=30, mu_CA=30, mu_CB=30, 
            sdYear=31.71, sdSite=0, sdSiteYear=27.07, sdResid=0))

results <- rbind(results,
            baci.power(n_TA=1, n_TB=1, n_CA=1, n_CB=1, 
            ns_T=1, ns_C=1, 
            ny_B=12, ny_A=13, 
            mu_TA=70, mu_TB=30, mu_CA=30, mu_CB=30, 
            sdYear=31.71, sdSite=0, sdSiteYear=27.07, sdResid=0))

results <- rbind(results,
            baci.power(n_TA=1, n_TB=1, n_CA=1, n_CB=1, 
            ns_T=1, ns_C=1, 
            ny_B=12, ny_A=13, 
            mu_TA=80, mu_TB=30, mu_CA=30, mu_CB=30, 
            sdYear=31.71, sdSite=0, sdSiteYear=27.07, sdResid=0))

results

sink("baci-fish-power-R-500.txt", split=TRUE) # make a small report
results[,c("alpha","ny_B","ny_A","baci","power")]
sink()
           

