"""The script that makes the cover figure.  Since this uses
matplotlib, it **cannot run under python3**.

"""

import sys
def main(argv=None):
    '''Call with arguments: data_dir, base_name, fig_name

    data_dir is the directory that has the state files

    base_name When it is "state" the data files are "state0", "state1"
    ,..., "state11".

    fig_name, eg, figs/Statesintro.pdf.  Where the figure gets written

    '''

    if sys.version_info < (3,0):
        import matplotlib as mpl
        mpl.use('PDF')
        import matplotlib.pyplot as plt
    else:
       print('%s needs matplotlib.  However, no matplotlib for python %s'%(
           sys.argv[0],sys.version_info,))
       return -1

    if argv is None:                    # Usual case
        argv = sys.argv[1:]

    data_dir, base_name, fig_name = argv
    plotcolor = [
        [1,0,0],       # Red
        [0,1,0],       # Green
        [0,0,1],       # Blue
        [0,1,1],       # Cyan
        [1,0,1],       # Magenta
        [.95,.95,0],  # Yellow
        [0,0,0]        # Black
        ]
    fig = plt.figure(figsize=(15,15))
    num = 0
    skiplist = [1,2,5,6]
    #The first loop is to graph each individual set of points, the second
    #is to get all of them at once.
    for b in range(0,12): # The last file is state11.
        name = '%s/%s%d'%(data_dir,base_name,b)
        xlist = []
        ylist = []
        zlist = []
        for line in open(name, 'r').readlines():#Read the data file
            x,y,z = [float(w) for w in line.split()]
            xlist.append(x)
            ylist.append(y)
            zlist.append(z)
        num += 1
        while num in skiplist: #This is to make space for putting in the
            num += 1           #figure with all the assembled pieces.
        
        ax = fig.add_subplot(4,4,num)
        # There are different subplots between here and the next
        # loop. One is 4x4 with smaller pieces, the other is a 2x2
        # simply made for positioning and sizing the completed piece.
        ax.set_xticks([])
        ax.set_yticks([])
        # Next, graph the x and z coordinates, with a color and point-type
        #(in this case pixels)
        ax.plot(xlist,zlist,color=plotcolor[b%7],marker=',',markersize=1,
                linestyle='None')
        ax.set_xlim(-20,20)
        ax.set_ylim(0,50)

    ax = fig.add_subplot(2,2,1)
    for b in range(0,12):
        name = '%s/%s%d'%(data_dir,base_name,b)
        xlist = []
        ylist = []
        zlist = []
        for line in open(name, 'r').readlines():#Read the data again
            x,y,z = [float(w) for w in line.split()]
            xlist.append(x)
            ylist.append(y)
            zlist.append(z)
        ax.set_xticks([])
        ax.set_yticks([])
        ax.plot(xlist,zlist,color=plotcolor[b%7],marker=',',markersize=2,
                linestyle='None')
    ax.set_xlim(-20,20)
    ax.set_ylim(0,50)
    fig.savefig(fig_name) #Make sure to save it as a .pdf
    return 0

if __name__ == "__main__":
    sys.exit(main())

# Local Variables:
# mode: python
# End:
