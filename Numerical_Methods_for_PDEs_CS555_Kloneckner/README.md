# Numerical Methods for Partial Differential Equations

Slides/notes and Jupyter notebook demos for an introductory course of numerical
methods for PDEs.

Originally developed for [CS555 in the spring of 2020](https://relate.cs.illinois.edu/course/cs555-s20/)
in the [Department of Computer Science](https://cs.illinois.edu/)
at the [University of Illinois](https://illinois.edu/), based on prior versions of the
class taught by Luke Olson.

-   [PDF](https://andreask.cs.illinois.edu/cs555-s20/notes.pdf) of these slides/notes (see `notes`
    for source)

-   [Demos](https://mybinder.org/v2/gh/inducer/numpde-notes/main?filepath=demos)
    in [Binder](https://mybinder.org/)

-   The demos use annotations for [`ipython-demo-tools`](https://github.com/inducer/ipython-demo-tools).
    A `#clear` annotation at the beginning of a code cell allows the
    `clear-marked-inputs` subcommand of
    [`prepare-ipynb`](https://github.com/inducer/ipython-demo-tools/blob/main/prepare-ipynb) 
    to remove the content of those input cells, maybe to use them for live coding in class.
    The `#clear` marks themselves can be removed by that `remove-marks` subcommand.

-   The notes are written in [Org mode](https://orgmode.org/), which serves as a lightweight
    markup language over LaTeX. They're easiest to edit in Emacs, but
    [vim-orgmode](https://github.com/jceb/vim-orgmode) will do as well.

    To build the notes, you need any recent version of Emacs installed. Also make sure that
    submodules cloned properly:
    ```
    git submodule update --init
    ```

    Then simply change to the `notes` subdirectory and say:
    ```
    ./make.sh
    ```
    The script will optionally make use of [`latexrun`](https://github.com/cjoach/latexrun).
    If you get
    ```
    There were errors; output not updated
    ```
    on the first go, simply rerun `make.sh`.

-   `make.sh` will generate two PDFs: `notes.pdf` and `notes-folded.pdf`.
    They differ in whether the boxes present in the notes (containing many
    of the most salient mathematical developments) are filled in or not.

    I use the un-filled version for class and fill in the boxes by hand
    during class using [Xournal++](https://github.com/xournalpp/xournalpp/)
    (but equivalent notetaking applications should work just as well).

## Problems and Assignments

If you can demonstrate that you are teaching a related course
at a college or university anywhere in the world, I have
homework assignments and projects (some of which are autograded
using the [RELATE](https://github.com/inducer/relate/) system)
that I would be happy to share with you.

## MIT License

Copyright (C) 2020 Andreas Kloeckner

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
