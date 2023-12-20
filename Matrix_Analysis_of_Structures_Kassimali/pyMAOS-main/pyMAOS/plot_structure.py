import matplotlib.pyplot as plt


def plot_structure(
    nodes,
    members,
    loadcombo,
    scaling={
        "axial_load": 100,
        "normal_load": 100,
        "point_load": 1,
        "axial": 2,
        "shear": 2,
        "moment": 0.1,
        "rotation": 5000,
        "displacement": 100,
    },
):
    # Plot the structure
    fig, axs = plt.subplots(2, 3, figsize=(8, 8))

    axial_loading_scale = scaling.get("axial_load", 1)
    normal_loading_scale = scaling.get("normal_load", 1)
    ptloading_scale = scaling.get("point_load", 1)
    axial_scale = scaling.get("axial", 1)
    shear_scale = scaling.get("shear", 1)
    moment_scale = scaling.get("moment", 1)
    rotation_scale = scaling.get("rotation", 1)
    displace_scale = scaling.get("displacement", 1)
    marker_shape = "o"
    marker_size = 2

    axs[0, 0].set_title(
        f"Geometry and Deformed Shape\n scale:{displace_scale}", fontsize=12
    )
    axs[0, 1].set_title(
        f"Member Loading \n axial scale:{axial_loading_scale} \n normal scale:{normal_loading_scale}\n pt load scale:{ptloading_scale}",
        fontsize=12,
    )

    axs[0, 2].set_title(f"Axial Force\n scale:{axial_scale}", fontsize=12)

    axs[1, 0].set_title(f"Shear Force\n scale:{shear_scale}", fontsize=12)

    axs[1, 1].set_title(f"Moment\n scale:{moment_scale}", fontsize=12)

    axs[1, 2].set_title(f"Cross-Section Rotation\n scale:{rotation_scale}", fontsize=12)

    for node in nodes:
        axs[0, 0].plot(node.x, node.y, marker=".", markersize=8, color="red")
        axs[0, 1].plot(node.x, node.y, marker=".", markersize=8, color="red")
        axs[0, 2].plot(node.x, node.y, marker=".", markersize=8, color="red")
        axs[1, 0].plot(node.x, node.y, marker=".", markersize=8, color="red")
        axs[1, 1].plot(node.x, node.y, marker=".", markersize=8, color="red")
        axs[1, 2].plot(node.x, node.y, marker=".", markersize=8, color="red")
        axs[0, 0].plot(
            node.x_displaced(loadcombo, displace_scale),
            node.y_displaced(loadcombo, displace_scale),
            marker=".",
            markersize=10,
            color="gray",
        )
    for member in members:
        axs[0, 0].plot(
            [member.inode.x, member.jnode.x],
            [member.inode.y, member.jnode.y],
            linewidth=1,
            color="black",
        )
        axs[0, 1].plot(
            [member.inode.x, member.jnode.x],
            [member.inode.y, member.jnode.y],
            linewidth=1,
            color="black",
        )
        axs[0, 2].plot(
            [member.inode.x, member.jnode.x],
            [member.inode.y, member.jnode.y],
            linewidth=1,
            color="black",
        )
        axs[1, 0].plot(
            [member.inode.x, member.jnode.x],
            [member.inode.y, member.jnode.y],
            linewidth=1,
            color="black",
        )
        axs[1, 1].plot(
            [member.inode.x, member.jnode.x],
            [member.inode.y, member.jnode.y],
            linewidth=1,
            color="black",
        )
        axs[1, 2].plot(
            [member.inode.x, member.jnode.x],
            [member.inode.y, member.jnode.y],
            linewidth=1,
            color="black",
        )
        aglobal = member.Aglobal_plot(loadcombo, axial_scale)
        dglobal = member.Dglobal_plot(loadcombo, displace_scale)

        axs[0, 2].plot(
            (aglobal[:, 0] + member.inode.x),
            (aglobal[:, 1] + member.inode.y),
            linewidth=1,
            color="blue",
            marker=marker_shape,
            markersize=marker_size,
        )

        axs[0, 0].plot(
            (dglobal[:, 0] + member.inode.x),
            (dglobal[:, 1] + member.inode.y),
            linewidth=1,
            color="gray",
            marker=marker_shape,
            markersize=marker_size,
        )

        if member.type != "TRUSS":
            vglobal = member.Vglobal_plot(loadcombo, shear_scale)
            mglobal = member.Mglobal_plot(loadcombo, moment_scale)
            sglobal = member.Sglobal_plot(loadcombo, rotation_scale)
            wxglobal = member.Wxglobal_plot(
                loadcombo, axial_loading_scale, ptloading_scale
            )
            wyglobal = member.Wyglobal_plot(
                loadcombo, normal_loading_scale, ptloading_scale
            )

            axs[0, 1].plot(
                (wxglobal[:, 0] + member.inode.x),
                (wxglobal[:, 1] + member.inode.y),
                linewidth=1,
                color="blue",
                marker=marker_shape,
                markersize=marker_size,
            )

            axs[0, 1].plot(
                (wyglobal[:, 0] + member.inode.x),
                (wyglobal[:, 1] + member.inode.y),
                linewidth=1,
                color="red",
                marker=marker_shape,
                markersize=marker_size,
            )

            axs[1, 0].plot(
                (vglobal[:, 0] + member.inode.x),
                (vglobal[:, 1] + member.inode.y),
                linewidth=1,
                color="green",
                marker=marker_shape,
                markersize=marker_size,
            )

            axs[1, 1].plot(
                (mglobal[:, 0] + member.inode.x),
                (mglobal[:, 1] + member.inode.y),
                linewidth=1,
                color="red",
                marker=marker_shape,
                markersize=marker_size,
            )
            axs[1, 1].plot(
                (member.inode.x, mglobal[0, 0] + member.inode.x),
                (member.inode.y, mglobal[0, 1] + member.inode.y),
                linewidth=1,
                color="red",
            )
            axs[1, 1].plot(
                (member.jnode.x, mglobal[-1, 0] + member.inode.x),
                (member.jnode.y, mglobal[-1, 1] + member.inode.y),
                linewidth=1,
                color="red",
            )

            axs[1, 2].plot(
                (sglobal[:, 0] + member.inode.x),
                (sglobal[:, 1] + member.inode.y),
                linewidth=1,
                color="purple",
                marker=marker_shape,
                markersize=marker_size,
            )

    axs[0, 0].grid(True)
    axs[0, 1].grid(True)
    axs[0, 2].grid(True)
    axs[1, 0].grid(True)
    axs[1, 1].grid(True)
    axs[1, 2].grid(True)

    axs[0, 0].set_aspect("equal", "box")
    axs[0, 1].set_aspect("equal", "box")
    axs[0, 2].set_aspect("equal", "box")
    axs[1, 0].set_aspect("equal", "box")
    axs[1, 1].set_aspect("equal", "box")
    axs[1, 2].set_aspect("equal", "box")

    fig.tight_layout()

    plt.show()
