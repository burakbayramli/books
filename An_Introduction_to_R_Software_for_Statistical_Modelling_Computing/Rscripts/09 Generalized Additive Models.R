######################## © CSIRO Australia 2005 ###############################
# Session 09:  GAMS: An Introduction                                          #
# Authors:     Petra Kuhnert & Bill Venables                                  #
#              CSIRO Mathematical and Information Sciences                    #
# Date:        28 November 2005                                               #
###############################################################################

######################
# Iowa Wheat Yield Data

# Initial Linear Model
Iowa <- read.csv("Iowa.csv")
iowa.lm1 <- lm(Yield ~ ., Iowa)
iowa.step <- stepAIC(iowa.lm1, scope = list(lower =  ~ Year, upper =  ~ .),
          k = log(nrow(Iowa)), trace = TRUE)
dropterm(iowa.step, test = "F", k = log(nrow(Iowa)),sorted = T)

# Additive Models
require(gam)
iowa.gam <- gam(Yield ~ s(Temp4) + s(Rain0) + s(Rain2) + s(Year),data = Iowa)
par(mfrow = c(2,2))
plot(iowa.gam, se = T, ylim = c(-30, 30), resid = T)
summary(iowa.gam)

# Additive Models through LMs
require(splines)
iowa.ns <- lm(Yield ~ ns(Temp4, df=3) + ns(Rain0, df=3) + ns(Rain2, df = 3) +
  ns(Year, df=3),Iowa)
# 'partial' says 'give me partial residuals'
tplot(iowa.ns, se = T, rug = T, partial = T)
dropterm(iowa.ns, test = "F", k = log(nrow(Iowa)))

############################
# Rock Dataset

# Linear Modelling
rock.lm <- lm(log(perm) ~ area + peri + shape, data = rock)
summary(rock.lm)

# Additive Modelling
rock.gam <- gam(log(perm) ~ s(area) + s(peri) + s(shape),
  control = gam.control(maxit = 50, bf.maxit = 50), data = rock)

summary(rock.gam)
anova(rock.lm, rock.gam) # shows no improvement

par(mfrow = c(2, 3), pty = "s")
tplot(rock.gam, se = TRUE,rug=TRUE)
rock.gam1 <- gam(log(perm) ~ area + peri + s(shape), data = rock)
tplot(rock.gam1, se = TRUE,rug=TRUE)
par(mfrow=c(1,1))

anova(rock.lm, rock.gam1, rock.gam)

# Simon Wood's Implementation
require(mgcv)
rock.gamSW <- gam(log(perm) ~ s(area) + s(peri) + s(shape), data = rock)






