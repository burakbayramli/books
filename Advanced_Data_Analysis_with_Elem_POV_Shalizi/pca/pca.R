######### First Example ##############
##### The American States in 1977 ####
# As done in class
  # The data file is built in to R

# Variables for the 50 states
summary(state.x77)

# Where R thinks the states are located
plot(state.center,type="n")
text(state.center,state.abb)

# First try at PCA of the states
state.pca1 <- prcomp(state.x77)

# Output summary:
print(state.pca1,digits=3)

# Scree plot
plot(state.pca1,type="l")

# Projection/biplot
biplot(state.pca1,cex=(0.75,1))

# Standard deviations of the variables
apply(state.x77,2,sd)


# Second try at PCA of the states
state.pca <- prcomp(state.x77,scale.=TRUE)

# Biplot: projections of states plus how the variables relate to the components
biplot(state.pca,cex=c(0.5,0.75))

# What are the first three components?
signif(state.pca$rotation[,1:3],3)

# Scree plot
plot(state.pca,type="l")

# function to plot the state abbrevations in position, with scaled sizes
  # Linearly scale the sizes from the given minimum to the maximum
# Inputs: vector of raw numbers, minimum size for plot,
  # maximum size
# Outputs: Rescaled sizes (invisible)
plot.states_scaled <- function(sizes,min.size=0.4,max.size=2,...) {
  out.range = max.size - min.size
  in.range = max(sizes)-min(sizes)
  scaled.sizes = out.range*((sizes-min(sizes))/in.range)
  sizes = scaled.sizes + min.size
  plot(state.center,type="n",...)
  text(state.center,state.abb,cex=sizes)
  invisible(sizes)
}

# PC1 plotted geographically
  # Arguably it's "Southerness"
plot.states_scaled(state.pca$x[,1],min.size=0.3,max.size=1.5,
                   xlab="longitude",ylab="latitude")


############# Second Example ##############
########### NY Times stories ##############
# New stories randomly selected from the New York Times Annotated Corpus
# 57 stories about art and 45 about music
# Load the stories from a saved workspace
  # Modify the location to be wherever you download the workspace
load("~/teaching/490/pca/pca-examples.Rdata")

  # The workspace now contains:
  # nyt.frame.raw: a data frame with counts of words (columns) in stories (rows)
  #                first column, "class.labels", is a factor indicating "art"
  #                or "music"
  # nyt.frame: the same, with word counts suitably normalized and weighted
  # art: vector where each row is itself a vector of words giving the
  #      actual stories about art, with punctuation removed, etc.
  # music: ditto
  # Some miscellaneous functions used to create the data sets (see end of
  # this file for gory details)

# We'll work with nyt.frame
# How big is it?
dim(nyt.frame)
  # Remember: rows = stories, columns = words (except the first column, which
  # is the type of story)
# What are some typical words?
colnames(nyt.frame)[sample(ncol(nyt.frame),30)]
# A little bit of the data
signif(nyt.frame[sample(nrow(nyt.frame),5),sample(ncol(nyt.frame),10)],3)
# What you should see: lots of zeroes! Most words do not appear in most stories.
# Harder to see: strong correlations between words which do appear.

# Do PCA
nyt.pca = prcomp(nyt.frame[,-1])
  # Omit the first column of class labels
# Extract the actual component directions/weights for ease of reference
nyt.latent.sem = nyt.pca$rotation

# What are the components?
  # Show the 30 words with the biggest positive loading on PC1
signif(sort(nyt.latent.sem[,1],decreasing=TRUE)[1:30],2)
  # biggest negative loading on PC1, the other end of that scale
signif(sort(nyt.latent.sem[,1],decreasing=FALSE)[1:30],2)
  # Ditto for PC 2
signif(sort(nyt.latent.sem[,2],decreasing=TRUE)[1:30],2)
signif(sort(nyt.latent.sem[,2],decreasing=FALSE)[1:30],2)

# Plot the projection of the stories on to the first 2 components
  # Establish the plot window
plot(nyt.pca$x[,1:2],type="n")
  # Arts stories with red As
points(nyt.pca$x[nyt.frame[,"class.labels"]=="art",1:2],pch="A",col="red")
  # Music stories with blue Ms
points(nyt.pca$x[nyt.frame[,"class.labels"]=="music",1:2],pch="M",col="blue")
# The separation is very good, even with only two components.

####################### The End ##################


# Actual creation of the pca-examples.Rdata file
  # Not much point to this unless you're me
  # The files referred to here are at www.stat.cmu.edu/~cshalizi/350/
# The NYT corpus is in XML
library(XML)
# Load my functions
source("~/teaching/350/hw/01/01.R")
# Load in the stories
art <- read.directory("~/teaching/350/hw/01/nyt_corpus/art")
music <- read.directory("~/teaching/350/hw/01/nyt_corpus/music")
# Turn each story into a bag of words
art.bow <- lapply(art,table)
music.bow <- lapply(music,table)
# Make a single data frame by combining the bags of words
nyt.frame.raw <- make.BoW.frame(c(art.bow,music.bow))

# Weight by inverse document frequency
nyt.frame <- idf.weight(nyt.frame.raw)
# Normalize by vector length
nyt.frame <- div.by.euc.length(nyt.frame)

# Add class labels
class.labels <- c(rep("art",57),rep("music",45))
nyt.frame.raw <- data.frame(class.labels=as.factor(class.labels),nyt.frame.raw)
nyt.frame <- data.frame(class.labels=as.factor(class.labels),nyt.frame)
  # the normalizing and weighting functions don't work well with non-
  # numeric columns so it's simpler to add the labels at the end
# Now save the workspace...
