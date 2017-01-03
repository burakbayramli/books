import numpy

def sim_lm(object, n_sims = 100):
    V_beta = object.normalized_cov_params
    sigma_hat = numpy.sqrt(object.mse_resid)
    print object.mse_resid
    beta_hat = object.params
    n = object.nobs
    k = object.nobs - object.df_resid
    sigma = numpy.nan * numpy.ones(n_sims)
    beta = numpy.nan *  numpy.ones((n_sims,k))
    for s in numpy.arange(n_sims):
        sigma[s] = sigma_hat * numpy.sqrt((n-k)/numpy.random.chisquare(n-k,1))
        beta[s,:] = numpy.random.multivariate_normal (beta_hat, V_beta*sigma[s]**2, 1)
    return beta, sigma

def sim_glm(res, n_sims = 100):
    beta_hat = res.params
    sd_beta = res.bse
    n = res.nobs
    k = int(res.nobs - res.df_resid)
    corr_beta = res.cov_params() / numpy.dot(res.bse[:,None],res.bse[None,:])

    V_beta = corr_beta * numpy.resize(sd_beta, (k,k)) * numpy.resize(sd_beta,(k,k)).T 
    beta = numpy.ones((n_sims, k)) * numpy.nan
    for s in numpy.arange(n_sims):
      beta[s,:] = numpy.random.multivariate_normal (beta_hat, V_beta, 1)
    
    dispersion = res.scale
    sigma = numpy.ones(n_sims) * numpy.sqrt(dispersion)     
        
    return beta, sigma


