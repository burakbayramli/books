# -*- coding: utf-8 -*-
import os

source = "source"
build = "build"
instr = "sphinx-build -b html "+source+" "+build
os.system(instr)
os.startfile(build+r"\index.html")
