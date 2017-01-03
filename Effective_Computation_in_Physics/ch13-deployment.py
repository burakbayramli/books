# Example setup.py

import sys
try:
    from setuptools import setup
    have_setuptools = True
except ImportError:
    from distutils.core import setup
    have_setuptools = False

setup_kwargs = {
    'name': 'compphys',
    'version': '0.1',
    'description': 'Effective Computation in Physics',
    'author': 'Anthony Scopatz and Kathryn D. Huff',
    'author_email': 'koolkatz@gmail.com',
    'url': 'http://www.oreilly.com/',
    'classifiers': [
        'License :: OSI Approved',
        'Intended Audience :: Developers',
        'Programming Language :: Python :: 3',
        ],
    'zip_safe': False,
    'packages': ['compphys', 'compphys.more'],
    'package_dir': {
        'compphys': 'compphys', 
        'compphys.more': 'compphys/more', 
        },
    'data_files': [('compphys/raw', ['*.txt'])],
    }

if __name__ == '__main__':
    setup(**setup_kwargs)

