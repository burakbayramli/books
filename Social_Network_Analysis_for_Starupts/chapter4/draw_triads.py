import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrowPatch, Circle
import triadic

def draw(G,pos,ax):
   for n in G:
       c=Circle(pos[n],radius=0.2,alpha=0.5)
       ax.add_patch(c)
       G.node[n]['patch']=c
   for u,v in G.edges():
       n1=G.node[u]['patch']
       n2=G.node[v]['patch']
       e = FancyArrowPatch(n1.center,n2.center,patchA=n1,patchB=n2,
                           arrowstyle='-|>',
                           connectionstyle='arc3,rad=0.2',
                           mutation_scale=10.0,
                           lw=1, alpha=0.5, color='k')
       ax.add_patch(e)
   ax.text(0.5,0.0,name,transform=ax.transAxes,horizontalalignment='center')
   ax.set_xlim(-1.0,3.0)
   ax.set_ylim(-0.5,1.5)
   plt.axis('equal')
   plt.axis('off')
   return 

t=triadic.triad_graphs()
n=len(t)
pos={'a':(0,0),'b':(1,1),'c':(2,0)}
fig=plt.figure(figsize=(8,2))
fig.subplots_adjust(left=0,right=1,top=1.0)
i=1
for name,graph in sorted(t.items()):
    ax=plt.subplot(2,n/2,i)
    draw(graph,pos,ax)
    i+=1
plt.savefig('triads.png')
plt.draw()
plt.show()
