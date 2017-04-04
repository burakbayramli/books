
# Contributing to OpenCV with Python Blueprints

**Note: This document is a 'getting started' summary for contributing code,
documentation, testing, and filing issues.**

How to contribute
-----------------

The preferred workflow for contributing to OpenCV with Python Blueprints is to fork the
[main repository](https://github.com/mbeyeler/opencv-python-blueprints) on
GitHub, clone, and develop on a branch. Steps:

1. Fork the [project repository](https://github.com/mbeyeler/opencv-python-blueprints)
   by clicking on the 'Fork' button near the top right of the page. This creates
   a copy of the code under your GitHub user account.

2. Clone your fork of the OpenCV with Python Blueprints repo from your GitHub account to your local disk:

   ```bash
   $ git clone https://github.com/YourLogin/opencv-python-blueprints.git
   $ cd opencv-python-blueprints
   ```

3. Create a ``feature`` branch to hold your development changes:

   ```bash
   $ git checkout -b my-feature
   ```

   Always use a ``feature`` branch. It's good practice to never work on the ``master`` branch!

4. Develop the feature on your feature branch. Add changed files using ``git add`` and then ``git commit`` files:

   ```bash
   $ git add modified_files
   $ git commit
   ```

   to record your changes in Git, then push the changes to your GitHub account with:

   ```bash
   $ git push -u origin my-feature
   ```

5. Go to the GitHub web page of your fork of the CARLsim 3 repo.
Click the 'Pull request' button to send your changes to the project's maintainers for
review. This will send an email to the committers.

(If any of the above seems like magic to you, please look up the
[Git documentation](https://git-scm.com/documentation) on the web, or ask a friend or another contributor for help.)

Pull Request Checklist
----------------------

We recommended that your contribution complies with the
following rules before you submit a pull request:

-  If your pull request addresses an issue, please use the pull request title
   to describe the issue and mention the issue number in the pull request description.
   This will make sure a link back to the original issue is created.

-  Please prefix the title of your pull request with `[MRG]` (Ready for
   Merge), if the contribution is complete and ready for a detailed review.
   uhAn incomplete contribution -- where you expect to do more work before
   receiving a full review -- should be prefixed `[WIP]` (to indicate a work
   in progress) and changed to `[MRG]` when it matures. WIPs may be useful
   to: indicate you are working on something to avoid duplicated work,
   request broad review of functionality or API, or seek collaborators.
   WIPs often benefit from the inclusion of a
   [task list](https://github.com/blog/1375-task-lists-in-gfm-issues-pulls-comments)
   in the PR description.

-  Documentation is necessary for enhancements to be accepted.


Filing bugs
-----------
We use Github issues to track all bugs and feature requests; feel free to
open an issue if you have found a bug or wish to see a feature implemented.

It is recommended to check that your issue complies with the
following rules before submitting:

-  Verify that your issue is not being currently addressed by other
   [issues](https://github.com/mbeyeler/opencv-python-blueprints/issues?q=)
   or [pull requests](https://github.com/mbeyeler/opencv-python-blueprints/pulls?q=).
   
-  Please include your operating system type and version number, as well as your Python,
   NumPy, SciPy, and OpenCV versions.
   This information can be found by runnning the following code snippet:
   
   ```
   import platform; print(platform.platform())
   import sys; print("Python", sys.version)
   import numpy; print("NumPy", numpy.__version__)
   import scipy; print("SciPy", scipy.__version__)
   import cv2; print("OpenCV", cv2.__version__)
