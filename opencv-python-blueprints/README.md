# OpenCV with Python Blueprints

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.154060.svg)](https://doi.org/10.5281/zenodo.154060)
[![Google group](https://img.shields.io/badge/Google-Discussion%20group-lightgrey.svg)](https://groups.google.com/d/forum/opencv-python-blueprints)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

This repository contains all up-to-date source code for the following book:

<img src="https://2.bp.blogspot.com/-0kv2Un_wtT4/VlX2XOazp3I/AAAAAAAAACE/bmZ6AsPfRKY8D6Btr10SObc6QiD8Hi0bQ/s200/2690OS_OpenCV%2Bwith%2BPython%2BBlueprints_.jpg" align="left" style="width: 220px; margin-right: 5px"/>
Michael Beyeler <br/>
<a href="http://www.amazon.com/OpenCV-Python-Blueprints-Michael-Beyeler/dp/1785282697"><b>OpenCV with Python Blueprints: Design and develop advanced computer vision projects using OpenCV with Python</b></a>

Packt Publishing Ltd., London, England <br/>
Paperback: 230 pages <br/>
ISBN 978-178528269-0
<br clear="both"/>

This book demonstrates how to develop a series of intermediate to advanced projects using OpenCV and Python,
rather than teaching the core concepts of OpenCV in theoretical lessons. Instead, the working projects
developed in this book teach the reader how to apply their theoretical knowledge to topics such as
image manipulation, augmented reality, object tracking, 3D scene reconstruction, statistical learning,
and object categorization.

By the end of this book, readers will be OpenCV experts whose newly gained experience allows them to develop their own advanced computer vision applications.

If you use either book or code in a scholarly publication, please cite as:
> M. Beyeler, (2015). OpenCV with Python Blueprints: Design and develop advanced computer vision projects using OpenCV with Python. Packt Publishing Ltd., London, England, 230 pages, ISBN 978-
178528269-0.

Or use the following bibtex:
```
@book{OpenCVWithPythonBlueprints,
	title = {{OpenCV with Python Blueprints}},
	subtitle = {Design and develop advanced computer vision projects using {OpenCV} with {Python}},
	author = {Michael Beyeler},
	year = {2015},
	pages = {230},
	publisher = {Packt Publishing Ltd.},
	isbn = {978-178528269-0}
}
```

All code was tested with OpenCV 2.4.9 and Python 2.7 on Ubuntu 14.04 and Windows 8.1, and is available from:
https://github.com/mbeyeler/opencv-python-blueprints.

For questions, discussions, and more detailed help please refer to the Google group:
https://groups.google.com/d/forum/opencv-python-blueprints


## Critical Reception

<img src="https://3.bp.blogspot.com/-m8yl8xCrM3Q/V9yFYMAj3YI/AAAAAAAAAq8/5IzGqAeUp9cCwq13j1EL7aunfUvvre5bQCLcB/s640/opencv-python-blueprints-amazon-new.png" style="width: 70%; margin-left: 15%"/>

What readers on Amazon have to say:

> The author does a great job explaining the concepts needed to understand what's happening in the application without 
> the need of going into too many details. <br/>
&ndash; [Sebastian Montabone](http://www.samontab.com)

> Excellent book to build practical OpenCV projects! I'm still relatively new to OpenCV, but all examples are well 
> laid out and easy to follow. The author does a good job explaining the concepts in detail and shows how they apply 
> in real life. As a professional programmer, I especially love that you can just fork the code from GitHub and follow 
> along. Strongly recommend to readers with basic knowledge of computer vision, machine learning, and Python!
&ndash; Amazon Customer

> Usually I'm not a big fan of technical books because they are too dull, but this one is written in an engaging 
> manner with a few dry jokes here and there. Can only recommend! <br/>
&ndash; lakesouth



## Who This Book Is for
As part of Packt's Blueprints series, this book is for intermediate users of OpenCV who aim to master their skills
by developing advanced practical applications. You should already have some
experience of building simple applications, and you are expected to be familiar with
OpenCV's concepts and Python libraries. Basic knowledge of Python programming
is expected and assumed.

By the end of this book, you will be an OpenCV expert, and your newly gained
experience will allow you to develop your own advanced computer vision
applications.



## Software Requirements
All projects can run on Windows, Mac, or Linux, and require the following software packages:
* OpenCV 2.4.9 or later: Recent 32-bit and 64-bit versions as well as installation instructions are available at
http://opencv.org/downloads.html. Platform-specific installation instructions can be found at
http://docs.opencv.org/doc/tutorials/introduction/table_of_content_introduction/table_of_content_introduction.html.
* Python 2.7 or later: Recent 32-bit and 64-bit installers are available at https://www.python.org/downloads. The
installation instructions can be found at https://wiki.python.org/moin/BeginnersGuide/Download.
* NumPy 1.9.2 or later: This package for scientific computing officially comes in 32-bit format only, and can be
obtained from http://www.scipy.org/scipylib/download.html. The installation instructions can be found at 
http://www.scipy.org/scipylib/building/index.html#building.

In addition, some chapters require the following free Python modules:
* wxPython 2.8 or later (Chapters 1 to 4, 7): This GUI programming toolkit can be obtained from
  http://www.wxpython.org/download.php.
  Its installation instructions are given at http://wxpython.org/builddoc.php.
  If you are using Max OS 10.11 (El Capitan), try:
  
  ```
  $ sudo pip install --upgrade --trusted-host wxpython.org --pre -f http://wxpython.org/Phoenix/snapshot-builds/ wxPython_Phoenix
  ```
  
  See [this bug](https://github.com/mbeyeler/opencv-python-blueprints/issues/9) for context.
  Thanks to @KaroAntonio for the fix!
* matplotlib 1.4.3 or later (Chapters 4 to 7): This 2D plotting library can be obtained from
  http://matplotlib.org/downloads.html. Its installation instructions can be found by going to
  http://matplotlib.org/faq/installing_faq.html#how-to-install.
* SciPy 0.16.0 or later (Chapter 1): This scientific Python library officially comes in 32-bit only, and can be
  obtained from http://www.scipy.org/scipylib/download.html. The installation instructions can be found at
  http://www.scipy.org/scipylib/building/index.html#building.
* libfreenect 0.5.2 or later (Chapter 2): The libfreenect module by the OpenKinect project (http://www.openkinect.org)
  provides drivers and libraries for the Microsoft Kinect hardware, and can be obtained from
  https://github.com/OpenKinect/libfreenect. Its installation instructions can be found at
  http://openkinect.org/wiki/Getting_Started.

Furthermore, the use of iPython (http://ipython.org/install.html) is highly recommended as it provides a flexible,
interactive console interface.

## License
The software is released under the GNU General Public License (GPL), which is the most commonly used free software
license according to Wikipedia. GPL allows for commercial use, distribution, modification, patent use, and private use.

The GPL is a copyleft license, which means that derived works can only be distributed under the same license terms.
For more information, please see the license file.
