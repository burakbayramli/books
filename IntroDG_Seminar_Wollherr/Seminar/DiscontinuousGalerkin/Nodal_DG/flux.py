#calculates the flux between two boundary sides of connected elements for
#element i
#General formula: 
#Flux matrix du
def flux(alpha,u,N,ne,mu):
#impose boundary conditions at x=0 and x=end
    ubd1=0 #
    ubd2=0 #
        
    du=np.zeros((N+1,ne)) # for every element we have 2 faces to other elements (left and right)
    for i in range(0, ne):
    

        if i==0: #left boundary
            du[0,i]= mu/2*(u[0,i]+ubd1) + (1-alpha)*abs(mu)/2*(ubd1-u[0,i]) #left flux
            du[N,i]= -mu/2*(u[N,i]+u[0,i+1]) - (1-alpha)*abs(mu)/2*(u[N,i]-u[0,i+1])#right flux

        elif i==ne-1:  #right boundary
            du[0,i]= mu*(u[0,i]+u[N,i-1])/2 + (1-alpha)*abs(mu)/2*(-u[0,i]+u[N,i-1])
            du[N,i]= -mu*(u[N,i]+ubd2)/2 - (1-alpha)*abs(mu)/2*(u[N,i]-ubd2)
            
        else: #in the middle of the domain
            du[0,i]= mu*(u[0,i]+u[N,i-1])/2 + (1-alpha)*abs(mu)/2*(-u[0,i]+u[N,i-1])
            du[N,i]= -mu*(u[N,i]+u[0,i+1])/2 - (1-alpha)*abs(mu)/2*(u[N,i]-u[0,i+1])
        


    return du

