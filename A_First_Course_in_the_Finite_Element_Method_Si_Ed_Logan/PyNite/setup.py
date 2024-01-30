import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="PyNiteFEA",
    version="0.0.87",
    author="D. Craig Brinck, PE, SE",
    author_email="Building.Code@outlook.com",
    description="A simple elastic 3D structural finite element library for Python.",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/JWock82/PyNite.git",
    packages=setuptools.find_packages(include=['PyNite', 'Pynite.*']),
    package_data = {'PyNite': ['*html', '*.css']},
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    install_requires=[
        'numpy',
        'PrettyTable'
    ],
    extras_require = {
        'Sparse Solver': ['scipy'],
        'Plotting': ['matplotlib'],
        'Visualization':  ['vtk'],
        'Visualization Screenshots': ['IPython'],
        'Reporting': ['pdfkit', 'Jinja2'],
        'Reviewing Derivations': ['jupyterlab', 'sympy']
    },
    include_package_data = True,
    python_requires = '>=3.6',
)
