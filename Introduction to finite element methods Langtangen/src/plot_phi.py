import matplotlib.pyplot as plt
from fe_approx1D import mesh_uniform, phi_glob
import numpy as np

def plot_fe_mesh(nodes, elements, element_marker=[0, 0.1]):
    """Illustrate elements and nodes in a finite element mesh."""
    plt.hold('on')
    all_x_L = [nodes[elements[e][0]] for e in range(len(elements))]
    element_boundaries = all_x_L + [nodes[-1]]
    for x in element_boundaries:
        plt.plot([x, x], element_marker, 'm--')  # m gives dotted lines
    plt.plot(nodes, [0]*len(nodes), 'ro')

def fe_basis_function_figure(d, target_elm=[1], N_e=3,
                             derivative=0, filename='tmp.pdf',
                             labels=False):
    """
    Draw all basis functions (or their derivative), of degree d,
    associated with element target_elm (may be list of elements).
    Add a mesh with N_e elements.
    """
    nodes, elements = mesh_uniform(N_e, d)
    """
    x = 1.1
    print locate_element_vectorized(x, elements, nodes)
    print locate_element_scalar(x, elements, nodes)
    x = 0.1, 0.4, 0.8
    print locate_element_vectorized(x, elements, nodes)
    """
    if isinstance(target_elm, int):
        target_elm = [target_elm]  # wrap in list

    # Draw the basis functions for element 1
    colors = ['r-2', 'b-2', 'g-2', 'y-2', 'k-2', 'c-2']
    color_counter = 0
    phi_drawn = []  # list of already drawn phi functions
    ymin = ymax = 0
    for e in target_elm:
        for i in elements[e]:
            if not i in phi_drawn:
                x, y = phi_glob(i, elements, nodes,
                                derivative=derivative)
                if x is None and y is None:
                    return  # abort
                ymax = max(ymax, max(y))
                ymin = min(ymin, min(y))
                plt.plot(x, y, colors[color_counter])
                color_counter += 1
                if color_counter > len(colors) - 1:
                    color_counter = 0  # restart colors

                if labels:
                    if derivative == 0:
                        plt.legend(r'\varphi_%d' % i, loc='upper right')
                    else:
                        plt.legend(r"\varphi_%d'(x)" % i, loc='upper right')
                phi_drawn.append(i)

    #plt.axis([nodes[0], nodes[-1], ymin-0.1, ymax+0.1])
    plot_fe_mesh(nodes, elements, element_marker=[ymin-0.1, ymax+0.1])
    plt.plot([0, 0], [0, 0], '-')
    plt.axis([nodes[0], nodes[-1], ymin-0.1, ymax+0.1])
    #plt.axes.set_aspect('')
    #plt.daspect([0.2, 1, 1])
    #plt.daspactmode('manual')
    plt.savefig(filename)

def draw_basis_functions():
    """Illustrate P1, P2, and P3 basis functions on an element."""
    for ext in 'pdf', 'png':
        for derivative in (0, 1):
            for labels in True, False:
                deriv = '' if derivative == 0 else 'd'
                philab = '' if not labels else '_lab'

                fe_basis_function_figure(
                    d=1, target_elm=1, N_e=4, derivative=derivative,
                    filename='fe_%sbasis_p1_4e%s.%s' % (deriv, philab, ext),
                    labels=labels)
                plt.figure()
                fe_basis_function_figure(
                    d=2, target_elm=1, N_e=4, derivative=derivative,
                    filename='fe_%sbasis_p2_4e%s.%s' % (deriv, philab, ext),
                    labels=labels)
                plt.figure()
                fe_basis_function_figure(
                    d=1, target_elm=1, N_e=5, derivative=derivative,
                    filename='fe_%sbasis_p1_5e%s.%s' % (deriv, philab, ext),
                    labels=labels)
                plt.figure()
                fe_basis_function_figure(
                    d=3, target_elm=1, N_e=4, derivative=derivative,
                    filename='fe_%sbasis_p3_4e%s.%s' % (deriv, philab, ext),
                    labels=labels)
                plt.figure()
                fe_basis_function_figure(
                    d=3, target_elm=2, N_e=5, derivative=derivative,
                    filename='fe_%sbasis_p3_5e%s.%s' % (deriv, philab, ext),
                    labels=labels)

def draw_sparsity_pattern(elements, num_nodes):
    """Illustrate the matrix sparsity pattern."""
    import matplotlib.pyplot as plt
    sparsity_pattern = {}
    for e in elements:
        for i in range(len(e)):
            for j in range(len(e)):
                sparsity_pattern[(e[i],e[j])] = 1
    y = [i for i, j in sorted(sparsity_pattern)]
    x = [j for i, j in sorted(sparsity_pattern)]
    y.reverse()
    plt.plot(x, y, 'bo')
    ax = plt.gca()
    ax.set_aspect('equal')
    plt.axis('off')
    plt.plot([-1, num_nodes, num_nodes, -1, -1], [-1, -1, num_nodes, num_nodes, -1], 'k-')
    plt.savefig('tmp_sparsity_pattern.pdf')
    plt.savefig('tmp_sparsity_pattern.png')
    plt.show()


def u_sines():
    """
    Plot sine basis functions and a resulting u to
    illustrate what it means to use global basis functions.
    """
    import matplotlib.pyplot as plt
    x = np.linspace(0, 4, 1001)
    psi0 = np.sin(2*np.pi/4*x)
    psi1 = np.sin(2*np.pi*x)
    psi2 = np.sin(2*np.pi*4*x)
    #u = 4*psi0 - 0.5*psi1 - 0*psi2
    u = 4*psi0 - 0.5*psi1
    plt.plot(x, psi0, 'r-', label=r"$\psi_0$")
    plt.plot(x, psi1, 'g-', label=r"$\psi_1$")
    #plt.plot(x, psi2, label=r"$\psi_2$")
    plt.plot(x, u, 'b-', label=r"$u=4\psi_0 - \frac{1}{2}\psi_1$")
    plt.legend()
    plt.savefig('u_example_sin.pdf')
    plt.savefig('u_example_sin.png')
    plt.show()

def u_P1():
    """
    Plot P1 basis functions and a resulting u to
    illustrate what it means to use finite elements.
    """
    import matplotlib.pyplot as plt
    x = [0, 1.5, 2.5, 3.5, 4]
    phi = [np.zeros(len(x)) for i in range(len(x)-2)]
    for i in range(len(phi)):
        phi[i][i+1] = 1
    #u = 5*x*np.exp(-0.25*x**2)*(4-x)
    u = [0, 8, 5, 4, 0]
    for i in range(len(phi)):
        plt.plot(x, phi[i], 'r-')  #, label=r'$\varphi_%d$' % i)
        plt.text(x[i+1], 1.2, r'$\varphi_%d$' % i)
    plt.plot(x, u, 'b-', label='$u$')
    plt.legend(loc='upper left')
    plt.axis([0, x[-1], 0, 9])
    plt.savefig('u_example_P1.png')
    plt.savefig('u_example_P1.pdf')
    # Mark elements
    for xi in x[1:-1]:
        plt.plot([xi, xi], [0, 9], 'm--')
    # Mark nodes
    #plt.plot(x, np.zeros(len(x)), 'ro', markersize=4)
    plt.savefig('u_example_P1_welms.png')
    plt.savefig('u_example_P1_welms.pdf')
    plt.show()


if __name__ == '__main__':
    import sys
    if len(sys.argv) == 1:
        print('Usage: %s phi | u_sines | u_P1 | pattern [num_elements] [d] [uniform | random]' % sys.argv[0])
        sys.exit(1)
    if sys.argv[1] == 'phi':
        draw_basis_functions()
    elif sys.argv[1] == 'pattern':
        num_elements = int(sys.argv[2])
        d = int(sys.argv[3])
        uniform = sys.argv[4]
        nodes, elements = mesh_uniform(num_elements, d, [0, 1])
        num_nodes = len(nodes)

        # Shuffle nodes in random order if necessary
        if uniform == 'random':
            global_node_numbering = list(range(0, num_nodes))
            import random
            random.shuffle(global_node_numbering)
            for e in range(len(elements)):
                for r in range(len(elements[e])):
                    elements[e][r] = global_node_numbering[elements[e][r]]
        draw_sparsity_pattern(elements, num_nodes)
    elif sys.argv[1] == 'u_sines':
        u_sines()
    elif sys.argv[1] == 'u_P1':
        u_P1()
