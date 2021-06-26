### Helpful functions


### trapezoidal rule for integrating
### f = (f(x_1), ..., f(x_n)) over x = (x_1, ..., x_n)
int.fun = function(f,x){
     n = length(x)
     h = diff(x)
     .5*sum(h*(f[-1]+f[-n]))
     }

