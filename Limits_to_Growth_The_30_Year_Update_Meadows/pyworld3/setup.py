#!/usr/bin/env python
# -*- coding: utf-8 -*-

from setuptools import setup

import pyworld3

description = "The World3 model revisited in Python."
setup(
    name='pyworld3',
    version=pyworld3.__version__,
    packages=["pyworld3"],
    description=description,
    long_description=description,

    author="Charles Vanwynsberghe",
    url='http://github.com/cvanwynsberghe/pyworld3',
    download_url="https://github.com/cvanwynsberghe/pyworld3/archive/v1.1.tar.gz",

    install_requires=["numpy", "scipy", "matplotlib"],

    include_package_data=True,  # files declared in MANIFEST.in

    classifiers=[
        "Programming Language :: Python",
        "Natural Language :: English",
        "Operating System :: OS Independent",
        "Topic :: Scientific/Engineering",
        "Topic :: Education",
        "Intended Audience :: Science/Research",
        "Intended Audience :: Education",
        "License :: OSI Approved :: CEA CNRS Inria Logiciel Libre License, version 2.1 (CeCILL-2.1)"
    ],

    license="CeCILL",
    )
