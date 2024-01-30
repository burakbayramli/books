# -*- coding: utf-8 -*-
# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  Web: numython.github.io
#  License: MIT License
# ***********************************
import os
from setuptools import setup


dir_setup = os.path.dirname(os.path.realpath(__file__))

with open(os.path.join(dir_setup, 'nusa', 'version.py')) as f:
    # Defines __version__
    exec(f.read())


with open("README.md", "r") as fh:
    long_description = fh.read()

# Current status: pre-alpha
setup(name='nusa',
      version=__version__,
      description='Numerical Structural Analysis in Python using the FEM',
      author='Pedro Jorge De Los Santos',
      author_email='delossantosmfq@gmail.com',
      license = "MIT",
      keywords=["Structural Analysis","Finite Element Analysis","Mechanical Engineering"],
      install_requires=["matplotlib","numpy","tabulate","meshio"],
      url='https://github.com/JorgeDeLosSantos/nusa',
      long_description=long_description,
      long_description_content_type="text/markdown",
      packages=['nusa'],
      classifiers=[
      "Development Status :: 2 - Pre-Alpha",
      "Intended Audience :: Education",
      "Intended Audience :: Developers",
      "License :: OSI Approved :: MIT License",
      "Operating System :: OS Independent",
      "Programming Language :: Python",
      "Programming Language :: Python :: 3",
      "Programming Language :: Python :: Implementation :: CPython",
      ]
      )