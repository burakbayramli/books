import numpy as np
import numpy.linalg as la

def read_shape():
    shapes = []
    for l in open("points.txt"):
        if l.startswith("m"):
            continue

        shape = []
        last_pt = None
        for point in l.split():
            if point == "m":
                shapes.append(np.array(shape))
                shape = []
                continue

            pt = np.array([float(s) for s in point.split(",")])
            pt *= np.array([1,-1])

            if last_pt is not None:
                ignore = la.norm(pt) < 4
                pt += last_pt
            else:
                ignore = False

            if not ignore:
                shape.append(pt)
            last_pt = pt

        shapes.append(np.array(shape))

    return shapes




def make_mesh(max_volume):
    def round_trip_connect(seq):
        result = []
        for i in range(len(seq)):
            result.append((i, (i+1)%len(seq)))
        return result

    shapes = read_shape()

    #from matplotlib.pyplot import plot,show
    #plot(shapes[0][:,0], shapes[0][:,1])
    #show()

    from meshpy.geometry import GeometryBuilder, Marker
    builder = GeometryBuilder()

    for shape in shapes:
        from meshpy.geometry import make_box
        points = shape
        facets = round_trip_connect(range(len(points)))
        builder.add_geometry(points=points, facets=facets,
                facet_markers=Marker.FIRST_USER_MARKER)

    points, facets, facet_markers = make_box((-200, -600), (400, -300))
    builder.add_geometry(points=points, facets=facets,
            facet_markers=facet_markers)

    def transform(pt):
        x, y = pt
        return -0.01*x, -0.01*y

    builder.apply_transform(transform)

    from meshpy.triangle import MeshInfo, build
    mi = MeshInfo()
    builder.set(mi)
    holes = []
    for shape, sign, frac in zip(shapes, [1, 1, -1, 1, 1, 1], [0.5, 0, 0, 0, 0, 0]):
        avg = np.average(shape, axis=0)
        start_idx = int(frac*shape.shape[0])
        start = shape[start_idx]
        holes.append(transform(start + sign*0.01*(avg-start)))

    mi.set_holes(holes)

    mesh = build(mi,
            allow_boundary_steiner=True,
            generate_faces=True,
            max_volume=max_volume)

    return mesh





if __name__ == "__main__":
    make_mesh(max_volume=0.004).write_neu(open("lettering-coarse.neu", "w"))
    make_mesh(max_volume=0.001).write_neu(open("lettering-fine.neu", "w"))
    make_mesh(max_volume=0.0004).write_neu(open("lettering-superfine.neu", "w"))

