# Helper functions that are commonly used in my course notes
#
# 2015-07-19 CJS fixed ggCatterpillar; but we now have a problem that arrangeGrob no longer returns a 
#                graphic object and you need to use grid.view() to display hit preceeded by a grid.newpage() - Groan
#                http://stackoverflow.com/questions/31463445/how-do-i-get-rid-of-random-background-grid-from-arrangegrob
# 2015-07-07 CJS updated plot.cld to recognize different CLD from lm() and glm()
# 2015-07-07 CJS added diagnostic plots for glm()
# 2014-11-29 CJS added sf.autopla.lmer() for diagnostic plots for lmer() models
# 2014-04-19 CJS sf.simple.summary


sf.simple.summary <- function(df, variable, crd=FALSE, conflevel=0.95){
  # Return simple statistics on 'variable' in the dataframe 'df'
  # If this is a crd design, also compute se and confidence intervals
  # This is commonly used in a ddply() function to get separate statistics for
  # each group.
  #
  data  <- df[,variable]
  n     <- length(data)
  nmiss <- sum(is.na(data)) # number of missing values
  mean  <- mean(data, na.rm=TRUE)
  sd    <- sd  (data, na.rm=TRUE)
  res   <- c(n, nmiss, mean, sd)
  res.names <- c("n","nmiss","mean","sd")
  # If this a CRD, compute the se and confint using a simple linear model
   if(crd){
     fit   <- lm(data ~1)
     se    <- sqrt(diag(vcov(fit)))
     ci    <- confint(fit, level=conflevel)
     res   <- c(res, se, ci)
     res.names <- c(res.names, "se","lcl",'ucl')
  }
  # put together all of the information
  names(res) <- res.names
  return(res)
}


# Create residual plots from lm() objects.
# See http://librestats.com/2012/06/11/autoplot-graphical-methods-with-ggplot2/
#     http://stackoverflow.com/questions/17059099/saving-grid-arrange-plot-to-file

# See http://rpubs.com/sinhrks/plot_lm for another autoplot function

sf.autoplot.lm <- function(model, ..., which=c(1:3, 5), mfrow=c(2,2)){
  require(ggplot2) 
  require(grid)
  require(gridExtra)
  df <- fortify(model)
  df <- cbind(df, rows=1:nrow(df))
 
  # residuals vs fitted
  g1 <- ggplot(df, aes(.fitted, .resid)) +
    geom_point()  +
    geom_smooth(se=FALSE) +
    geom_hline(linetype=2, size=.2) +
      scale_x_continuous("Fitted Values") +
      scale_y_continuous("Residual") +
      ggtitle("Residuals vs Fitted")
 
  # normal qq
  a <- quantile(df$.stdresid, c(0.25, 0.75))
  b <- qnorm(c(0.25, 0.75))
  slope <- diff(a)/diff(b)
  int <- a[1] - slope * b[1]
  g2 <- ggplot(df, aes(sample=.stdresid)) +
    stat_qq() +
    geom_abline(slope=slope, intercept=int) +
      xlab("Theoretical Quantiles") +
      ylab("Standardized Residuals") +
      ggtitle("Normal Q-Q")
 
  # scale-location
  g3 <- ggplot(df, aes(.fitted, sqrt(abs(.stdresid)))) +
    geom_point() +
    geom_smooth(se=FALSE) +
      scale_x_continuous("Fitted Values") +
      scale_y_continuous("Root of Standardized Residuals") +
      ggtitle("Scale-Location")
 
  # cook's distance
  g4 <-  ggplot(df, aes(rows, .cooksd, ymin=0, ymax=.cooksd)) +
    geom_point() + geom_linerange() +
      scale_x_continuous("Observation Number") +
      scale_y_continuous("Cook's distance") +
      ggtitle("Cook's Distance")
 
  # residuals vs leverage
  g5 <- ggplot(df, aes(.hat, .stdresid)) +
    geom_point() +
    geom_smooth(se=FALSE) +
    geom_hline(linetype=2, size=.2) +
      scale_x_continuous("Leverage") +
      scale_y_continuous("Standardized Residuals") +
      ggtitle("Residuals vs Leverage")
 
  # cooksd vs leverage
  g6 <- ggplot(df, aes(.hat, .cooksd)) +
    geom_point() +
    geom_smooth(se=FALSE) +
      scale_x_continuous("Leverage") +
      scale_y_continuous("Cook's distance") +
      ggtitle("Cook's dist vs Leverage")
 
  #browser()
  plots <- list(g1, g2, g3, g4, g5, g6)
  plots.subset <- plots[which]
  plots.subset$ncol <- mfrow[2]
  plots.subset$nrow <- mfrow[1]
  
  gridplots <- do.call(arrangeGrob, plots.subset)
  gridplots  # return the final object
}

# Create residual and other diagnostic plots from lmer() objects.
sf.autoplot.lmer <- function(model, ..., which=TRUE, mfrow=c(2,2)){
  # which = TRUE implies select all plots; specify a vector if only want some of the plots
  require(ggplot2) 
  require(grid)
  require(gridExtra)
  require(lattice)
  require(plyr)
  
  ggCaterpillar <- function(re, QQ=TRUE, likeDotplot=TRUE) {
    # Create Caterpillar plots
    # Refer to http://stackoverflow.com/questions/13847936/in-r-plotting-random-effects-from-lmer-lme4-package-using-qqmath-or-dotplot
    # We modified it to access the name of the random effect for use in the plots
    # http://stackoverflow.com/questions/9950144/access-lapply-index-names-inside-fun
    
    require(ggplot2)
    f <- function(i, allre) {
      re_name <- names(allre)[i] # name of the random effect
      x   <- allre[[i]]
      pv   <- attr(x, "postVar")
      cols <- 1:(dim(pv)[1])
      se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
      ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
      pDf  <- data.frame(y=unlist(x)[ord],
                         ci=1.96*se[ord],
                         nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                         ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                         ind=gl(ncol(x), nrow(x), labels=names(x)))
      
      if(QQ) {  ## normal QQ-plot
        p <- ggplot(pDf, aes(nQQ, y))
        p <- p + facet_wrap(~ ind, scales="free")
        p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
      } else {  ## caterpillar dotplot
        p <- ggplot(pDf, aes(ID, y)) + coord_flip()
        if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
          p <- p + facet_wrap(~ ind)
        } else {           ## different scales for random effects
          p <- p + facet_grid(ind ~ ., scales="free_y")
        }
        p <- p + xlab("Levels") + ylab("Random effects") + ggtitle(paste("Caterpillar Plot of ", re_name))
      }
      
      p <- p + theme(legend.position="none")
      p <- p + geom_hline(yintercept=0)
      p <- p + geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour="black")
      p <- p + geom_point(aes(size=1.2), colour="blue") 
      return(p)
    }
    res<- lapply(seq_along(re), f, allre=re)
    names(res) <- names(re)
    res
  }
  
  
  
  df <- fortify(model)
  df <- cbind(df, rows=1:nrow(df))
  
  # residuals vs fitted
  g1 <- ggplot(df, aes(x=.fitted, y=.resid)) +
    geom_point()  +
    geom_smooth(se=FALSE) +
    geom_hline(yintercept=0, linetype=2, size=.2) +
    scale_x_continuous("Fitted Values") +
    scale_y_continuous("Residual") +
    ggtitle("Residuals vs Fitted")
  
  # normal qq on residuals
  a <- quantile(df$.resid, c(0.25, 0.75))
  b <- qnorm(c(0.25, 0.75))
  slope <- diff(a)/diff(b)
  int <- a[1] - slope * b[1]
  g2 <- ggplot(df, aes(sample=.resid)) +
    stat_qq() +
    geom_abline(slope=slope, intercept=int) +
    xlab("Theoretical Quantiles") +
    ylab("Residuals") +
    ggtitle("Normal Q-Q on residuals")
  
  # caterpillar plots on the all of the random effects
  cat_plot <- ggCaterpillar( ranef(model, condVar=TRUE),  QQ=FALSE, likeDotplot=FALSE)
  
  plots <- list(g1=g1, g2=g2)
  cat_names <- names(cat_plot)
  l_ply(cat_names, function(name){
    # add the caterpiller plots to the list of plots
    #browser()
    plots <<- c(plots, cat_plot[name])
  })
  plots.subset <- plots[which]
  plots.subset$ncol <- mfrow[2]
  #plots.subset$nrow <- mfrow[1]
  # browser()
  gridplots <- do.call(arrangeGrob, plots.subset)
  gridplots  # return the final object
}


# Create residual plots from glm() objects.
# See http://rpubs.com/sinhrks/plot_lm
if (!"devtools" %in% installed.packages()) install.packages("devtools")
library(devtools)
if (!"ggfortify" %in% installed.packages()) install_github('sinhrks/ggfortify')
library(ggfortify)
sf.autoplot.glm <- function(...){ggplot2::autoplot(...)}


sf.cld.plot.bar<- function(cld.obj, variable, order=TRUE, whereCLD=0.20, ciwidth=0.2){
  # Create a ggplot object of the cld as a bar graph
  # You can add axes labels as needed after plot creating
  #    cld.obj  - cld object created by lsmeans
  #    variable - name of grouping varible (usually the first column in the cld.obj
  #    order    - plot bars sorted from smallest to largest)
  #    whereCLD - where should the cld letters be plotted as a proportion of y axis from bottom
  #
  # See where the lower and upper confidence limits are from lm() and glm() objects respectively
  lcl.col <- which(grepl('lower.CL', names(cld.obj)) | grepl('asymp.LCL', names(cld.obj)) )
  ucl.col <- which(grepl('upper.CL', names(cld.obj)) | grepl('asymp.UCL', names(cld.obj)) )
  if(order) { cld.obj$gf <- factor(cld.obj[,variable], cld.obj[,variable])} # sorted levels 
  if(!order){ cld.obj$gf <- cld.obj[,variable]}
  require(ggplot2)
  plot <- ggplot(cld.obj, aes(x=gf,y=lsmean), environment=environment())+
             geom_bar(stat="identity", alpha=0.5)+
             geom_errorbar( aes(ymax=cld.obj[,lcl.col], ymin=cld.obj[,ucl.col]), width=ciwidth)
  
   # Extract the range of the y axis to decide where to annotate the cld values
  yrange <- ggplot_build(plot)$panel$ranges[[1]]$y.range
  plot <- plot +
          annotate("text", 
            x=cld.obj$gf,
            y=yrange[1]+whereCLD*sum(c(-1,1)*yrange),
            label=cld.obj$".group", angle=-90,vjust=1)
  plot
} # end of sf.cld.plot.bar


sf.cld.plot.line<- function(cld.obj, variable, order=TRUE, whereCLD=0.20, ciwidth=0.20){
  # Create a ggplot object of the cld as a line graph
  # You can add axes labels as needed after plot creating
  #    cld.obj  - cld object created by lsmeans
  #    variable - name of grouping varible (usually the first column in the cld.obj
  #    order    - plot bars sorted from smallest to largest)
  #    whereCLD - where should the cld letters be plotted as a proportion of y axis from bottom
  # See where the lower and upper confidence limits are from lm() and glm() objects respectively
  lcl.col <- which(grepl('lower.CL', names(cld.obj)) | grepl('asymp.LCL', names(cld.obj)) )
  ucl.col <- which(grepl('upper.CL', names(cld.obj)) | grepl('asymp.UCL', names(cld.obj)) )
  
  if(order) { cld.obj$gf <- factor(cld.obj[,variable], cld.obj[,variable])} # sorted levels 
  if(!order){ cld.obj$gf <- cld.obj[,variable]}
  require(ggplot2)
  plot <- ggplot(cld.obj, aes(x=gf,y=lsmean), environment=environment())+
             geom_line(aes(group=1))+
             geom_errorbar( aes(ymax=cld.obj[,lcl.col], ymin=cld.obj[,ucl.col]), width=ciwidth)
  
   # Extract the range of the y axis to decide where to annotate the cld values
  yrange <- ggplot_build(plot)$panel$ranges[[1]]$y.range
  plot <- plot +
          annotate("text", 
            x=cld.obj$gf,
            y=yrange[1]+whereCLD*sum(c(-1,1)*yrange),
            label=cld.obj$".group", angle=-90,vjust=1)
  plot
} # end of sf.cld.plot.line