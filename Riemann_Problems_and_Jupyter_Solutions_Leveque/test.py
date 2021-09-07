import os
import subprocess
import tempfile
import sys
import nbformat
import doctest
import utils

if sys.version_info >= (3,0):
    kernel = 'python3'
else:
    kernel = 'python2'

def _notebook_run(path):
    """Execute a notebook via nbconvert and collect output.
       :returns (parsed nb object, execution errors)
    """
    with tempfile.NamedTemporaryFile(suffix=".ipynb") as fout:
        args = ["jupyter", "nbconvert", "--to", "notebook", "--execute",
                "--ExecutePreprocessor.timeout=60",
                "--ExecutePreprocessor.kernel_name="+kernel,
                "--output", fout.name, path]
        subprocess.check_call(args)

        fout.seek(0)
        nb = nbformat.reads(fout.read().decode('utf-8'), nbformat.current_nbformat)

    errors = [output for cell in nb.cells if "outputs" in cell
              for output in cell["outputs"]
              if output.output_type == "error"]

    return nb, errors

def run_tests():
    doctest.testmod(utils.riemann_tools)

    # We don't test this notebook because we don't
    # install the required ffmpeg library on Travis.
    ignore_notebooks = ['Make_html_animations.ipynb']

    for filename in os.listdir('.'):
        if (filename.split('.')[-1] == 'ipynb' and
                    filename not in ignore_notebooks):
            _, errors = _notebook_run(filename)
            if errors != []:
                raise(Exception)


if __name__ == '__main__':
    run_tests()
