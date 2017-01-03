###############################################################################
# Read in the subset data and get set up for modeling
###############################################################################

apt.subset.data <- read.table ("apt.subset.dat", header=TRUE)
attach.all (apt.subset.data)
y <- y.subset
defects <- defects.subset
poor <- poor.subset
race <- race.subset
floor <- floor.subset
dist <- dist.subset
bldg <- bldg.subset

asian <- race==5 | race==6 | race==7
black <- race==2
hisp <- race==3 | race==4

n <- length (y)
n.bldg <- max (bldg)
n.dist <- max (dist)

bldg.subset.data <- read.table ("bldg.subset.dat", header=TRUE)
attach.all (bldg.subset.data)

dist.data <- read.table ("dist.dat", header=TRUE)
attach.all (dist.data)
