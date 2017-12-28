import numpy as np
import matplotlib.pyplot as plt
#From Strogatz p.133, Fig. 5.2.1
#He_dot = heloves + sheloveshimback
#She_dot = sheloves + helovesherback
def phaseplot(he_loves=5,she_responds=-6,she_loves=2,he_responds=8, s1=2,s2=2):
    plt.close()
    x,y = np.linspace(-10,10,100),np.linspace(-10,10,100)
    He,She = np.meshgrid(x,y)
    A = np.array([[he_loves,she_responds],[she_loves,he_responds]])

    U = A[0,0]*He +A[0,1]*She
    V = A[1,0]*She + A[1,1]*He
    #Eigenvector calculations.
    w,v = np.linalg.eig(A)
    v = np.round(v,2)
    w = np.round(w,2)
    eigvects = np.dot(A,v)

    speed = np.sqrt(U*U + V*V)

    start = [[s1,s2]]

    fig0, ax0 = plt.subplots(1,1,figsize=(8,8))
    ax0.set_aspect('equal')
    strm = ax0.streamplot(x,y, U, V, color=(.75,.90,.93), linewidth=.5)
    strmS = ax0.streamplot(x,y, U, V, start_points=start, color="crimson", linewidth=1)
    ax0.plot(start[0][0],start[0][1],'go')
    ax0.set_xlim(-10,10)
    ax0.set_ylim(-10,10)
    try:
        ax0.plot(x,eigvects[1,0]/eigvects[0,0]*x,'b',alpha=.7)
    except RuntimeError:
        print("Division by zero error: No meaningful eigenvector")
    try:
        ax0.plot(x,eigvects[1,1]/eigvects[0,1]*x,'b',alpha=.7)
    except RuntimeError:
        print("Division by zero error: No meaningful eigenvector")
    ax0.set_title('Linear System', size=14)
    ax0.set_xlabel('Him',size=14)
    ax0.set_ylabel(r'Her',size=14 )
    ax0.text(-9,8,r'$\dot{0}$ = {1}x + {2}y'.format('x',A[0,0],A[0,1]))
    ax0.text(-9,7,'$\dot{0}$ = {1}x +{2} y'.format('y',A[1,0],A[1,1]))
    ax0.text(-9,6,r'$(x_0,y_0)$ = ({0}, {1})'.format(start[0][0],start[0][1]))
    ax0.text(-9,5,'$\lambda$ = {0}, {1} '.format(w[0],w[1]))
    ax0.text(-9,4,'Eigenvectors = {0}, {1}'.format(v[0].T,v[1].T))
    ax0.text(-9,-9,'General solution = c_1{0}e^({1}t) + c_2{2}e^({3}t)$'.format(v[0],w[0],v[1],w[1]))
    plt.grid()
    plt.show()
phaseplot()
