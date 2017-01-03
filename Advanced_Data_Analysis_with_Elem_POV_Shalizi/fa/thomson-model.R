# Simulate Godfrey Thomson's "sampling model" of mental abilities, and perform
# factor analysis on the resulting test scores.


# MASS package used to make random multivariate normal vectors
require(MASS)


# Simulate the Thomson model
  # Follow Thomson's original sampling-without-replacement scheme
  # Pick a random number in 1:a for the number of shared abilities for each test
  # Then draw a sample-without-replacement of that size from 1:a; those are the
  # shared abilities summed in that test.
  # Specific variance of each test is also random; draw a number in 1:q, and
  # sum that many independent normals, with the same parameters as the
  # abilities.
# Inputs: number of testees, number of tests, number of shared abilities, number
#     of specific abilities per test, mean of each ability, sd of each ability
# Calls: mvrnorm from library MASS (multivariate random normal generator)
# Outputs: matrix of test loadings on to general abilities, vector of number of
#     specific abilities per test, matrix of abilities-by-testees, matrix of
#     general+specific scores by testees, raw data (including measurement noise)
rthomson <- function(n,d,a,q,ability.mean=0,ability.sd=1) {
    # Using incomprehensible parameter names is bad
    # number of testees = n
    # number of tests = d
    # number of shared abilities = a
    # max. number of specific abilities per test = q

    # assign abilities to tests
    general.per.test = floor(runif(d,min=0,max=a)) + 1
    specifics.per.test = floor(runif(d,min=0,max=q)) + 1

    # Define the matrix assigning abilities to tests
    general.to.tests = matrix(0,a,d)
    # The use of a for loop here is maybe a little slower than some sort
    # of vectorization, but it's sanity-preserving; so is using the temporary
    # variable "abilties" to hold the sample.
    for (i in 1:d) {
        abilities = sample(1:a,size=general.per.test[i],replace=FALSE)
        general.to.tests[abilities,i] = 1
    }

    # Covariance matrix of the general abilities
    sigma = matrix(0,a,a)
    diag(sigma) = (ability.sd)^2
    mu=rep(ability.mean,a)
    x = mvrnorm(n,mu,sigma) # person-by-abilities matrix of abilities

    # The "general" part of the tests
    general.tests = x %*% general.to.tests

    specific.tests = matrix(0,n,d)
    noisy.tests = matrix(0,n,d)
    # Each test gets its own specific abilities, which are independent for each
    # person
    # Again, I could probably vectorize, but d is small and this is saner
    for (i in 1:d) {
        # Each test has noises.per.test disturbances, each of which has the
        # given sd; since these are all independent their variances add
        j = specifics.per.test[i]
        specifics = rnorm(n,mean=ability.mean*j,sd=ability.sd*sqrt(j))
        specific.tests[,i] = general.tests[,i] + specifics
        # Finally, for extra realism, some mean-zero trial-to-trial noise, so
        # that if we re-use this combination of general and specific ability
        # scores, we won't get the exact same test scores twice
        noises = rnorm(n,mean=0,sd=ability.sd)
        noisy.tests[,i] = specific.tests[,i] + noises
    }

     tm = list(data=noisy.tests,general.ability.pattern = general.to.tests,
              numbers.of.specifics = specifics.per.test,
              ability.matrix = x, specific.tests = specific.tests)
     return(tm)
}