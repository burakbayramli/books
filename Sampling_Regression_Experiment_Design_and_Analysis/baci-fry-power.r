# Power of the BACI-fry example
#   2013-12-17 CJS Fixed to account for changes in lme4() package

# This illustrates the power computations for a BACI design with multiple treatment
# and control sites, multiple years measured before and after treatment is applied, and
# sub-sampling at each year-site combination.

# The proposed experimental design is a BACI experiment with 
# the  monitoring design has the river divided into sites of which 
# some are upstream of the diversion and some are downstream of the diversion. 
# In each site, several minnow traps are set for various lengths of time. 
#
# At the end of the soaking period, the traps are removed and the number of 
# minnows are counted and classified by species.
# The counts are standardized to a common period of time to adjust for the 
# different soak-times. 
#
# This is run for several years before the project starts and after the project starts

# The analysis was originally done on the log-scale from which
# the variance components were extracted (see baci-fry.r code)
# 
# We want to do a power analysis looking at various aspects of the design
#
# The information we need is:
#     Alpha level (usually .05 or .10)
#     Variance components (these were obtained from the baci-fry.r analysis of the previous data)
#        std_site  -> site to site STANDARD DEVIATION
#        std_year  -> year to year STANDARD DEVIATION
#        std_site_year -> site-year interaction STANDARD DEVIATION
#        std_subsample -> within site sub-sampling STANDARD DEVIATION
#     Sub-sampling sample sizes, i.e. the number of minnow traps (sub-samples) set at each site
#       We allow for different sample sizes in the treatment-time combinations
#        but all sites in that combination must have the same number of minnow traps.
#        It is possible to generalize this; contact me for details.
#        n_TA   -> number of subsamples in Treatment-After  combination
#        n_TB   -> number of subsamples in Treatment-Before combination
#        n_CA   -> number of subsamples in Control-After    combination
#        n_CB   -> number of subsamples in Control-Before   combination
#     Number of segments(sites)
#        We allow for a different number of sites for Treatment and Control areaa
#        ns_T     -> number of treatment sites (i.e. downstram of the project)
#        ns_C     -> number of control sites (i.e. upstream of the project)
#     Number of years of monitoring before/after impact
#        ny_B     -> number of years monitoring before project starts
#        ny_A     -> number of years monitoring after  project starts
#     Marginal Means
#        These are used to form the BACI contrast of interest
#        mu_TA, mu_TB, mu_CA, mu_CB (i.e mean of Treatment-After,
#        Treatment-Before, Control-After, Control_Before)
#        These are chosen based on the size of impact that may be biologically important
 
# This code was originally created by 
#    Tony Booth
#    Department of Ichthyology and Fisheries Science, 
#    Rhodes University, 
#    PO Box 94, 
#    Grahamstown 6140 SOUTH AFRICA
#    t.booth@ru.ac.za

# The computations are based on the 
#    Stroup, W. W. (1999)
#    Mixed model procedures to assess power, precision, and sample size in the design of experiments.
# paper where "dummy" data is generated and "analyzed" and the resulting F-statistics etc
# provide information needed to compute the power

library(lme4)


#-----------------------------------------------

##***part500b;
# Get the BACI power program
source("../baci-power.r")

# An illustration of how to find the power for one scenario
baci.power(n_TA=3, n_TB=3, n_CA=3, n_CB=3,
            ns_T=3, ns_C=3, 
            ny_B=3, ny_A=2, 
            mu_TA=5.0, mu_TB=5.5, mu_CA=4.5, mu_CB=4.5, 
            sdYear=0.25, sdSite=0.75, sdSiteYear=0.1, sdResid=0.75)

#find the power under various scenarios
results <- NULL
results <- rbind(results,
            baci.power(n_TA=3, n_TB=3, n_CA=3, n_CB=3,
            ns_T=3, ns_C=3, 
            ny_B=3, ny_A=2, 
            mu_TA=5.0, mu_TB=5.5, mu_CA=4.5, mu_CB=4.5, 
            sdYear=0.25, sdSite=0.75, sdSiteYear=0.1, sdResid=0.75))
##***part500e;

# Do more scenarios 
results <- rbind(results,
            baci.power(n_TA=6, n_TB=6, n_CA=6, n_CB=6, 
            ns_T=3, ns_C=3, 
            ny_B=3, ny_A=2, 
            mu_TA=5.0, mu_TB=5.5, mu_CA=4.5, mu_CB=4.5, 
            sdYear=0.25, sdSite=0.75, sdSiteYear=0.1, sdResid=0.75))
            
results <- rbind(results,
            baci.power(n_TA=9, n_TB=9, n_CA=9, n_CB=9, 
            ns_T=3, ns_C=3, 
            ny_B=3, ny_A=3, 
            mu_TA=5.0, mu_TB=5.5, mu_CA=4.5, mu_CB=4.5, 
            sdYear=0.25, sdSite=0.75, sdSiteYear=0.1, sdResid=0.75))
            
results <- rbind(results,
            baci.power(n_TA=6, n_TB=6, n_CA=6, n_CB=6, 
            ns_T=3, ns_C=3, 
            ny_B=3, ny_A=4, 
            mu_TA=5.0, mu_TB=5.5, mu_CA=4.5, mu_CB=4.5, 
            sdYear=0.25, sdSite=0.75, sdSiteYear=0.1, sdResid=0.75))

results

sink("baci-fry-power-R-500.txt", split=TRUE)
results[,c("alpha","n_TA","n_TB","n_CA","n_CB","ny_B","ny_A","ns_T","ns_C","baci","power")]
sink()         

