import networkx as nx
import matplotlib.pyplot as plt

G = nx.MultiDiGraph()
G.position = {}
G.labels = {}

nlevels = 5
dx = 0.2
dy = 0.2

fig = plt.figure()
ax = fig.add_subplot(111)

# down

for n in range(nlevels-1):
    node = f"d{n}"
    print(node)
    G.add_node(node)
    G.position[node] = (n*dx, -n*dy)
    if n == 0:
        G.labels[node] = r"$\nabla^2 \phi^h = f^h$"
    else:
        G.labels[node] = rf"$\nabla^2 e^{{{2**n}h}} = r^{{{2**n}h}}$"

# bottom
node = "bottom"
G.add_node(node)
G.position[node] = ((nlevels-1)*dx, -(nlevels-1)*dy)
G.labels[node] = "bottom solve"

# up

for n in range(nlevels-1):
    node = f"u{nlevels-2-n}"
    print(node)
    G.add_node(node)
    G.position[node] = ((nlevels + n)*dx, -(nlevels-2-n)*dy)
    if n == nlevels-2:
        G.labels[node] = r"$\nabla^2 \phi^h = f^h$"
    else:
        G.labels[node] = rf"$\nabla^2 e^{{{2**(nlevels-2-n)}h}} = r^{{{2**(nlevels-2-n)}h}}$"


# now do the edges
for n in range(nlevels-2):
    G.add_edges_from([(f"d{n}", f"d{n+1}")])

G.add_edges_from([(f"d{nlevels-2}", "bottom")])
G.add_edges_from([("bottom", f"u{nlevels-2}")])

for n in range(nlevels-2):
    start = f"u{nlevels-2-n}"
    stop = f"u{nlevels-3-n}"
    print(start, stop)
    G.add_edges_from([(start, stop)])

print(G.position)

nx.draw(G, G.position,      # plot the element at the correct position
        node_color="C1", alpha=1.0,
        node_shape="o", node_size=500,
        width=2, linewidths=2.0, ax=ax)

edges = [e for e in G.edges]
edge_labels = {}
for e in edges:
    if e[0].startswith("d"):
        edge_labels[(e[0], e[1])] = "restrict"
    else:
        edge_labels[(e[0], e[1])] = "prolong"

nx.draw_networkx_edge_labels(G, G.position, edge_labels=edge_labels,
                             font_size="12", font_color="C0")

nx.draw_networkx_labels(G, G.position, labels=G.labels)

ax.axis("off")
fig.set_size_inches(8, 8)

fig.tight_layout()
fig.savefig("vcycle.png", dpi=100)
