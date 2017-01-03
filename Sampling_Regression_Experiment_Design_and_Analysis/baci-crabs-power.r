# Power analysis for simple BACI analysis
# 2014-11-26 CJS update split; fixed ##*** problems



# For a simple BACI there is 
#   1 site in each of treatment and control -> ns_T=1, ns_C=1
#   1 year before/after                     -> ny_B=1, ny_A=1
# We cannot separate the site-year interaction from the means so we hope
# for the best and suppose that sdSiteYear=0

# Example of crabs.
# Density is about 35 and a BACI difference of 5 is important
#    We use mu_TA=25, mu_TB=30, mu_CB=35, mu_cA=35
#
# We don't have to worry about the sdSite or the sdYear because these "concel"
# because every site is measured every year.
# The residual standard deviation is about 5
#
# We examine various scenarios for the number of subsamples at each of the 4 site-year combinations


# Get the BACI power program
source("../baci-power.r")


# illustration of how the power program work for a specific scenario
baci.power(n_TA=5, n_TB=5, n_CA=5, n_CB=5, 
            ns_T=1, ns_C=1, 
            ny_B=1, ny_A=1, 
            mu_TA=30, mu_TB=35, mu_CA=35, mu_CB=35, 
            sdYear=0, sdSite=0, sdSiteYear=0, sdResid=3.32)



##***part500b;
results <- NULL
results <- rbind(results,
            baci.power(n_TA=5, n_TB=5, n_CA=5, n_CB=5, 
            ns_T=1, ns_C=1, 
            ny_B=1, ny_A=1, 
            mu_TA=30, mu_TB=35, mu_CA=35, mu_CB=35, 
            sdYear=0, sdSite=0, sdSiteYear=0, sdResid=3.32))

results <- rbind(results,
            baci.power(n_TA=10, n_TB=10, n_CA=10, n_CB=10, 
            ns_T=1, ns_C=1, 
            ny_B=1, ny_A=1, 
            mu_TA=30, mu_TB=35, mu_CA=35, mu_CB=35, 
            sdYear=0, sdSite=0, sdSiteYear=0, sdResid=3.32))

results <- rbind(results,
            baci.power(n_TA=15, n_TB=15, n_CA=15, n_CB=15, 
            ns_T=1, ns_C=1, 
            ny_B=1, ny_A=1, 
            mu_TA=30, mu_TB=35, mu_CA=35, mu_CB=35, 
            sdYear=0, sdSite=0, sdSiteYear=0, sdResid=3.32))
            
results <- rbind(results,
            baci.power(n_TA=20, n_TB=20, n_CA=20, n_CB=20, 
            ns_T=1, ns_C=1, 
            ny_B=1, ny_A=1, 
            mu_TA=30, mu_TB=35, mu_CA=35, mu_CB=35, 
            sdYear=0, sdSite=0, sdSiteYear=0, sdResid=3.32))
##***part500e;

results

sink("baci-crabs-power-R-500.txt", split=TRUE)
results[,c("alpha","n_TA","n_TB","n_CA","n_CB","baci","power")]
sink()
           

