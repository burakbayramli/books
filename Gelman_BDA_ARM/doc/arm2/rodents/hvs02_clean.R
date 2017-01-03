###############################################################################
# Clean and prepare the data for multilevel modeling
###############################################################################

# Read in unsorted data and (approximately) create building index variable

hvs02.unsorted <- read.csv ("hvs02_unsorted.csv")
attach.all (hvs02.unsorted)
n <- nrow (hvs02.unsorted)
sequenceno.unsorted <- sequenceno

x.bldg <- cbind (numunits, stories, cd, extwin4_2, extflr5_2, old, dilap)
bldg.unsorted <- rep (0,n)
bldg.unsorted[1] <- 1
for (i in 2:n){
  bldg.unsorted[i] <- bldg.unsorted[i-1] +
    ifelse (identical (x.bldg[i-1,], x.bldg[i,]) & sum(bldg.unsorted==bldg.unsorted[i-1])<4, 0, 1) 
}
n.bldg <- max(bldg.unsorted)

# How many buildings are there in the dataset with 1,2,3,4 apts?

table (table (bldg.unsorted))
#    1    2    3    4 
# 5405 1580  867 1182

# Read in full (sorted) data set

hvs02.sorted <- read.csv ("hvs02_sorted.csv")
attach.all (hvs02.sorted)
n <- nrow (hvs02.sorted)

# Define variables of interest

y <- rodent2
n <- length (y)
dist <- cd

# Line up the building ID's to the "sequenceno" variable

bldg <- rep (0,n)
for (i in 1:n){
  bldg[i] <- bldg.unsorted[sequenceno.unsorted==sequenceno[i]]
}

# Get district numbers for the buildings

n.bldg <- max(bldg)
bldg.dist <- rep (NA, n.bldg)
for (j in 1:n.bldg){
  dist.j <- unique(dist[bldg==j])
  if (length(dist.j)>1) stop ("not all apts in this bldg are in the same dist")
  bldg.dist[j] <- dist.j
}

###############################################################################
# Define an "apt/bldg defects score" and a "poverty score"
###############################################################################

# Impute missing values for the predictors:  for each, impute avg for other
# apts in the bldg (if available), otherwise impute the avg in the district

impute <- function (a){
  n <- length(a)
  missing <- is.na(a)
  a.imp <- a
  for (i in (1:n)[missing]){
    if (sum (bldg==bldg[i] & !missing) > 0)
      a.imp[i] <- mean (a[bldg==bldg[i]], na.rm=T)
    else
      a.imp[i] <- mean (a[dist==dist[i]], na.rm=T)
  }
  return (a.imp)
}

floor <- impute(unitflr2)
defects <- impute(extwin4_2) + impute(extflr5_2) + impute(intcrack2) +
  impute(inthole2) + impute(intleak2) + impute(intpeel_cat) + impute(dilap) +
  (1-impute(struct))
poor <- impute(poverty) + impute(povertyx2) + impute(board2) +
  impute(subsidy) + impute(ifelse (housing==3, 1, ifelse (housing==4, 2,
                         ifelse (housing==2, 3, ifelse (housing==1, 4, NA)))))

###############################################################################
# Define defects and poverty scores at the district level
###############################################################################

# Define district defects and poverty scores for each apt (no missingness here)

dist.defects.raw <- extwin4_2_Mean + extflr5_2_Mean + inthole2_Mean + intleak2_Mean + dilap_Mean + (1-struct_Mean)
dist.poor.raw <- poverty_Mean + povertyx2_Mean + pubhous_Mean + (1-ownhous_Mean) + board2_Mean

# To get district scores, compute averages over the apartments within each district

group.mean <- function (a, id){
  n.group <- max(id)
  output <- rep (NA, n.group)
  for (j in 1:n.group){
    output[j] <- mean (a[id==j])
  }
  return (output)
}
dist.defects <- group.mean (dist.defects.raw, dist)
dist.poor <- group.mean (dist.poor.raw, dist)

###############################################################################
# Divide the districts into strata
###############################################################################

n.dist <- max (dist)
n.strata <- 10
n.per.stratum <- n.dist/n.strata + .001
stratum <- floor (rank(dist.poor)/n.per.stratum) + 1
dist.renumbered <- rep (NA, n.dist)
for (j in 1:n.strata){
  dist.renumbered[stratum==j] <- 1:sum(stratum==j)
}
dist.id <- dist.renumbered[dist]
dist.stratum <- stratum[dist]

###############################################################################
# Save the full apt-level data into 1 file, "apt_all.dat"
###############################################################################

apt.all.data <- cbind (hvs02.sorted, bldg, dist, dist.stratum, dist.id)
write.table (apt.all.data, "apt_all.dat")
rodents.data <- cbind (hvs02.sorted, bldg)
write.table (rodents.data, "rodents.dat")

###############################################################################
# Save the data into 3 files, "apt.dat", "bldg.dat", and "dist.dat"
###############################################################################

apt.data <- cbind (y, defects, poor, race, floor, dist, bldg)
bldg.data <- cbind (bldg.dist)
dist.data <- cbind (dist.defects, dist.poor)
write.table (apt.data, "apt.dat")
write.table (bldg.data, "bldg.dat")
write.table (dist.data, "dist.dat")

###############################################################################
# To speed the computations (to get started), just work with a random sample
# of 1000 bldgs
###############################################################################

n.keep <- 1000
bldg.sample <- sort (sample (n.bldg, n.keep))
apt.include <- rep (FALSE,n)
for (i in 1:n.keep){
  apt.include[bldg==bldg.sample[i]] <- TRUE
}

write.table (rodents.data[apt.include,], "rodents.dat")

y.subset <- y[apt.include]
defects.subset <- defects[apt.include]
poor.subset <- poor[apt.include]
race.subset <- race[apt.include]
floor.subset <- floor[apt.include]
dist.subset <- dist[apt.include]

# Renumber the buildings (for convenience in later computation)

bldg.include <- bldg[apt.include]
bldg.subset <- rep (NA, sum(apt.include))
for (i in 1:length(bldg.sample)){
  bldg.subset[bldg.include==bldg.sample[i]] <- i
}

###############################################################################
# Save the subset data into 2 files, "apt.subset.dat" and "bldg.subset.dat".
# (No need for "dist.subset.dat" since the subset includes data from all dists
###############################################################################

apt.subset.data <- cbind (y.subset, defects.subset, poor.subset, race.subset, floor.subset, dist.subset, bldg.subset)
bldg.subset.data <- cbind (bldg.dist[bldg.include])
write.table (apt.subset.data, "apt.subset.dat")
write.table (bldg.subset.data, "bldg.subset.dat")

attach.overwrite <- function(a, overwrite=TRUE, name="attach.all"){
#
# Version of the "attach" function that overwrites variables if necessary.
# (The usual "attach" function in R only attaches the variables that are not
# yet defined.)
#
  if (overwrite){
    for (j in 1:length(a)){
      if (names(a)[j] %in% ls(.GlobalEnv))
        remove (list=names(a)[j], envir=.GlobalEnv)
    }
  }
  attach (a, name=name)
}

