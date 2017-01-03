# Power analysis for BACI design with multiple sites, but one year before/after
# 2014-11-26 CJS ggplot, split=true, etc

# Example of crab - modifieds.
# Density is about 35 and a BACI difference of 5 is important
#    We use mu_TA=25, mu_TB=30, mu_CB=35, mu_cA=35
#
# We don't have to worry about the sdSite or the sdYear because these "concel"
# because every site is measured every year.
# The residual standard deviation is about 3 32
#
# We examine various scenarios for the number of sites and number subsamples 

# Get the BACI power program
source("../baci-power.r")


# An illustration of how to run for single case
baci.power(n_TA=5, n_TB=5, n_CA=5, n_CB=5, 
            ns_T=1, ns_C=2, 
            ny_B=1, ny_A=1, 
            mu_TA=30, mu_TB=35, mu_CA=35, mu_CB=35, 
            sdYear=0, sdSite=3.831, sdSiteYear=1.296, sdResid=3.30)



##***part500b;
results <- NULL
results <- rbind(results,
            baci.power(n_TA=5, n_TB=5, n_CA=5, n_CB=5, 
            ns_T=1, ns_C=2, 
            ny_B=1, ny_A=1, 
            mu_TA=30, mu_TB=35, mu_CA=35, mu_CB=35, 
            sdYear=0, sdSite=3.831, sdSiteYear=1.296, sdResid=3.30))
            
# More scenarios not listed            

##***part500e;

results <- rbind(results,
            baci.power(n_TA=40, n_TB=40, n_CA=40, n_CB=40, 
            ns_T=1, ns_C=2, 
            ny_B=1, ny_A=1, 
            mu_TA=30, mu_TB=35, mu_CA=35, mu_CB=35, 
            sdYear=0, sdSite=3.831, sdSiteYear=1.296, sdResid=3.30))

results <- rbind(results,
            baci.power(n_TA=5, n_TB=5, n_CA=5, n_CB=5, 
            ns_T=1, ns_C=4, 
            ny_B=1, ny_A=1, 
            mu_TA=30, mu_TB=35, mu_CA=35, mu_CB=35, 
            sdYear=0, sdSite=3.831, sdSiteYear=1.296, sdResid=3.30))

results <- rbind(results,
            baci.power(n_TA=20, n_TB=20, n_CA=20, n_CB=20, 
            ns_T=1, ns_C=4, 
            ny_B=1, ny_A=1, 
            mu_TA=30, mu_TB=35, mu_CA=35, mu_CB=35, 
            sdYear=0, sdSite=3.831, sdSiteYear=1.296, sdResid=3.30))

results <- rbind(results,
            baci.power(n_TA=20, n_TB=20, n_CA=5, n_CB=5, 
            ns_T=1, ns_C=4, 
            ny_B=1, ny_A=1, 
            mu_TA=30, mu_TB=35, mu_CA=35, mu_CB=35, 
            sdYear=0, sdSite=3.831, sdSiteYear=1.296, sdResid=3.30))

results <- rbind(results,
            baci.power(n_TA=5, n_TB=5, n_CA=5, n_CB=5, 
            ns_T=1, ns_C=10, 
            ny_B=1, ny_A=1, 
            mu_TA=30, mu_TB=35, mu_CA=35, mu_CB=35, 
            sdYear=0, sdSite=3.831, sdSiteYear=1.296, sdResid=3.30))

results <- rbind(results,
            baci.power(n_TA=5, n_TB=5, n_CA=5, n_CB=5, 
            ns_T=2, ns_C=10, 
            ny_B=1, ny_A=1, 
            mu_TA=30, mu_TB=35, mu_CA=35, mu_CB=35, 
            sdYear=0, sdSite=3.831, sdSiteYear=1.296, sdResid=3.30))

results <- rbind(results,
            baci.power(n_TA=40, n_TB=40, n_CA=40, n_CB=40, 
            ns_T=2, ns_C=10, 
            ny_B=1, ny_A=1, 
            mu_TA=30, mu_TB=35, mu_CA=35, mu_CB=35, 
            sdYear=0, sdSite=3.831, sdSiteYear=1.296, sdResid=3.30))

results

sink("baci-crabs-mod-power-R-500.txt", split=TRUE) # get a smaller report
results[,c("alpha","ns_T","ns_C","n_TA","n_TB","n_CA","n_CB","baci","power")]
sink()
           

