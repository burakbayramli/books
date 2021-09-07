import sys, os


def osher_solution(f, q_left, q_right, n=1000):
    """
    Compute the Riemann solution to a scalar conservation law.
    
    Compute the similarity solution Q(x/t) and also the 
    (possibly multi-valued) solution determined by tracing 
    characteristics.
    
    Input:
      f = flux function (possibly nonconvex)
      q_left, q_right = Riemann data
      
    Returns:
      qtilde = function of xi = x/t giving the Riemann solution
    """

    from numpy import linspace,empty,argmin,argmax
    
    q_min = min(q_left, q_right)
    q_max = max(q_left, q_right)
    qv = linspace(q_min, q_max, n)
    
    # define the function qtilde as in (16.7)
    if q_left <= q_right:
        def qtilde(xi):
            Q = empty(xi.shape, dtype=float)
            for j,xij in enumerate(xi):
                i = argmin(f(qv) - xij*qv)
                Q[j] = qv[i]
            return Q
    else:
        def qtilde(xi):
            Q = empty(xi.shape, dtype=float)
            for j,xij in enumerate(xi):
                i = argmax(f(qv) - xij*qv)
                Q[j] = qv[i]
            return Q
    
    return qtilde

def nonconvex_solutions(f, q_left, q_right, xi_left=None, xi_right=None):
    """
    Compute the Riemann solution to a scalar conservation law.
    
    Compute the similarity solution Q(x/t) and also the 
    (possibly multi-valued) solution determined by tracing 
    characteristics.
    
    Input:
      f = flux function (possibly nonconvex)
      q_left, q_right = Riemann data
      xi_left, xi_right = optional left and right limits for xi = x/t
               in similarity solution.
               If not specified, chosen based on the characteristic speeds.
    
    Returns:
      xi = array of values between xi_left and xi_right
      q  = array of corresponding q(xi) values (xi = x/t)
      q_char = array of values of q between q_left and q_right
      xi_char = xi value for each q_char for use in plotting the
              (possibly multi-valued) solution where each q value
              propagates at speed f'(q).
    """
    
    from numpy import linspace,diff,hstack
    
    qtilde = osher_solution(f, q_left, q_right)
    
    q_min = min(q_left, q_right)
    q_max = max(q_left, q_right)
    qv = linspace(q_min, q_max, 1000)
    
     
    xi = linspace(xi_left, xi_right, 1000)
    q = qtilde(xi)
    
    # The rest is just for plotting purposes:
    fv = f(qv)
    dfdq = diff(fv) / (qv[1] - qv[0])
    dfdq_min = dfdq.min()
    dfdq_max = dfdq.max()
    dfdq_range = dfdq_max - dfdq_min
    
    #print("Mininum characteristic velocity: %g" % dfdq_min)
    #print("Maximum characteristic velocity: %g" % dfdq_max)
    
    if xi_left is None: 
        xi_left = min(0,dfdq_min) - 0.1*dfdq_range
    if xi_right is None: 
        xi_right = max(0,dfdq_max) + 0.1*dfdq_range
        
    q_char = hstack((q_min, 0.5*(qv[:-1] + qv[1:]), q_max))
    
    if q_left <= q_right:
        xi_min = xi_left
        xi_max = xi_right
    else:
        xi_min = xi_right
        xi_max = xi_left
   
    xi_char = hstack((xi_min, dfdq, xi_max))
    
    return xi, q, q_char, xi_char
