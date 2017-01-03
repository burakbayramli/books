# BACI Power function
# 2014-02-26 CJS Added control () to the lmer() call to avoid problems with no sub-samples
# 2013-02-04 CJS Added code to compute the one-sided power for the specified BACI contrast as well
# 2013-12-16 CJS Modified code to deal with changes from new release of lmer()

# This function computes the power for a BACI design with multiple treatment
# and control sites, multiple years measured before and after treatment is applied, and
# sub-sampling at each year-site combination.
#
# The information we need is:
#     Alpha level (usually .05 or .10)
#     Variance components (these were obtained from the an analysis of previous data)
#        sdSite      -> site to site STANDARD DEVIATION
#        sdYear      -> year to year STANDARD DEVIATION
#        sdSiteYear  -> site-year interaction STANDARD DEVIATION
#        sdResid     -> within site sub-sampling STANDARD DEVIATION
#     Sub-sampling sample sizes, i.e. the number of sub-samples taken at each site-period combinations.
#       We allow for different sample sizes in the treatment-time combinations
#        but all sites in that combination must have the same number of samples.
#        It is possible to generalize this; contact me for details.
#        n_TA   -> number of subsamples in Treatment-After  combination
#        n_TB   -> number of subsamples in Treatment-Before combination
#        n_CA   -> number of subsamples in Control-After    combination
#        n_CB   -> number of subsamples in Control-Before   combination
#     Number of sites
#        We allow for a different number of sites for Treatment and Control areaa
#        ns_T     -> number of treatment sites (i.e. downstram of the project)
#        ns_C     -> number of control sites (i.e. upstream of the project)
#     Number of years of monitoring before/after impact
#        ny_B     -> number of years monitoring before project starts
#        ny_A     -> number of years monitoring after  project starts
#     Marginal Means
#        These are used to form the BACI contrast of interest
#        mu_TA, mu_TB, mu_CA, mu_CB (i.e mean of Treatment-After,
#        Treatment-Before, Control-After, Control_before)
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

baci.power <- function(n_TA,n_TB,n_CA,n_CB,ns_T,ns_C,ny_B,ny_A,mu_TA,mu_TB,mu_CA,mu_CB,
                      sdYear, sdSite, sdSiteYear, sdResid, alpha=0.05){
# This computes the power of the baci-fry example. Arguments are defined above

# First generate a "dummy" dataset of the appropriate size with the response variable
# set to the mean as needed. Arguments of the function were defined above.      
  
#  Total number ob observations
   n <- ns_T*ny_A*n_TA+   # treatment sites x years after  x traps/site/year
        ns_T*ny_B*n_TB+   # treatment sites x years before x traps/site/year
        ns_C*ny_A*n_CA+   # control   sites x years after  x traps/site/year
        ns_C*ny_B*n_CB    # control   sites x years before x traps/site/year
   
   time <- treatment <- sites <- years <- mu <- rep(0,n)
   
    m <- 1
	for(site in 1:ns_T)
		{
		 for(year in 1:ny_A)
	         for(quadrat in 1:n_TA)
	            {	
	            	sites[m] <- paste("LT",site,sep="")
	            	years[m] <- paste("TA",year,sep="")
	            	treatment[m] <- "I"
	            	time[m] <- "A"
	            	mu[m] <- mu_TA
	            	m <- m+1
	            }
		  for(year in 1:ny_B)
		     for(quadrat in 1:n_TB)
		     	{
		     		sites[m] <- paste("LT",site,sep="")
	            	years[m] <- paste("TB",year,sep="")
		     		treatment[m] <- "I"
	            	time[m] <- "B"
	            	mu[m] <- mu_TB
	            	m <- m+1
		     	}
      }
      
   for(site in 1:ns_C)
		{
		 for(year in 1:ny_A)
	         for(quadrat in 1:n_CA)
	            {
	            	sites[m] <- paste("LC",site,sep="")
	            	years[m] <- paste("TA",year,sep="")
	            	treatment[m] <- "C"
	            	time[m] <- "A"
	            	mu[m] <- mu_CA
	            	m <- m+1
	            }
			    
		 for(year in 1:ny_B)
		     for(quadrat in 1:n_CB)
		     	{
		     		sites[m] <- paste("LC",site,sep="")
	            	years[m] <- paste("TB",year,sep="")
		     		treatment[m] <- "C"
	            	time[m] <- "B"
	            	mu[m] <- mu_CB
	            	m <- m+1
		     	} 
      }   
   
  
  testdata <- data.frame(CI=treatment, BA=time, L=sites, Time=years, mu)
  
# Now to fit the data and extract the various bits of interest
# We don't actually need the results, but we want the various matrices
  fit <- lmer(mu ~ -1 + CI:BA + (1|L) + (1|Time) + (1|L:Time), data = testdata,
   control=lmerControl(
                       check.nobs.vs.rankZ = "ignore",
                       check.nobs.vs.nlev  = "ignore", 
                       check.nlev.gtreq.5  = "ignore",
                       check.nlev.gtr.1    = "ignore",
                       check.nobs.vs.nRE   = "ignore"))
  
  n <- nrow(testdata)
  nL <- ns_T+ns_C     # number of sites for the control and treatment
  nT <- ny_A + ny_B   # number of times before and after
  nLT <- nL*nT

# Extract details from the unfitted object, ...
#   browser()
   Y <- getME(fit, "y") # the response variable
   X <- matrix(getME(fit, "X"), nrow=n)
   zmat <- getME(fit,"Ztlist") # returns list of random effect matrices - transposed
   Location     <- zmat[[which(grepl("^L\\.\\("   ,names(zmat)))]]   #random effects design matrix for Sites
   Time         <- zmat[[which(grepl("^Time\\.\\(",names(zmat)))]]
   LocationTime <- zmat[[which(grepl("^L:Time"    ,names(zmat)))]]

# Now, fit the model and summarise
# Get fixed effects and fixed effects varcovar matrix
  beta <- solve(t(X)%*%X)%*%t(X)%*%Y

# now, add the variance components together to generate the variance-covariance matrix
  V <- diag(sdResid^2,n,n) + t(Time)%*%Time*sdYear^2 + t(Location)%*%Location*sdSite^2 + 
       t(LocationTime)%*%LocationTime*sdSiteYear^2

# the contrast vector for the BACI effect
  K <- c(1,-1,-1,1)
  baci=mu_TA-mu_TB-mu_CA+mu_CB

#  calculate the non-centrality parameter, and then the power
  ncp <- as.numeric(t(t(K)%*%beta)%*%solve(t(K)%*%solve(t(X)%*%solve(V)%*%X)%*%K)%*%(t(K)%*%beta))
  dfdenom <- (ny_B-1 + ny_A-1)*(ns_T-1 + ns_C-1) +  # site*time(BACI) term
             (ny_B-1 + ny_A-1)*1 +                  # BA*site(CI) term
             (ns_T-1 + ns_C-1)*1                    # CI*time(BA) term
  if(dfdenom==0){ # simple baci design with
  	 dfdenom = n-1-3;
  }           

  Fcrit <- qf(1-alpha, 1, dfdenom)
  power <- 1 - pf(Fcrit, 1, dfdenom,ncp)

#  Compute the one-sided power, i.e. to detect the change in one direction only
#  Because I don't know which direction of interest, the one-sided power is 
#  computed in both directions.
#
  Tcrit <- qt(1-alpha,dfdenom)
  os.power1 <- 1-pt(Tcrit,dfdenom,sqrt(ncp))
  os.power2 <- pt(-Tcrit,dfdenom,sqrt(ncp))

  return(list(alpha=alpha, 
     sdSite=sdSite, sdYear=sdYear, sdSiteYear=sdSiteYear, sdResid=sdResid,
     n_TA=n_TA, n_TB=n_TB, n_CA=n_CA,n_CB=n_CB,
     ns_T=ns_T, ns_C=ns_C, ny_B=ny_B, ny_A=ny_A, 
     mu_TA=mu_TA, mu_TB=mu_TB, mu_CA=mu_CA, mu_CB=mu_CB, 
     baci=baci,
     dfdenom=dfdenom, ncp=ncp, Fcrit=Fcrit, power=power,
     Tcrit=Tcrit, os.power1=os.power1, os.power2=os.power2))
}

