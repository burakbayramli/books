
# To use Docker

**Note:** In the instructions below, 
 - `$` refers to the native bash shell prompt on your computer, 
 - `jovyan $` refers to the bash shell prompt within the Docker container, once you have that running.

First install [Docker](https://www.docker.com/).  If it is installed, make sure it is running.

Then do:

    $ docker run -i -t -p 8889:8889 --name rbook_container clawpack/rbook

This starts a virtual machine (*container*) named `rbook_container` based on the docker image `clawpack/rbook` that is available on [dockerhub](https://hub.docker.com/r/clawpack/rbook).

If the container starts properly, you will now see the `jovyan $` prompt in your shell window.  You are in a bash shell and can move around and examine files as desired.

### Start the notebook server

To start a Jupyter notebook server that is serving these Riemann notebooks, execute the following in the container:

    jovyan $ jupyter notebook --ip=0.0.0.0 --port=8889 --no-browser

The port number 8889 should agree with the port number you
specified in the `docker run` command. This is the port on
which the notebooks will be served.

In the output of this command, you should see a URL to copy paste
into a browser to start your notebook, such as:

     To access the notebook, open this file in a browser ...
     Or copy and paste one of these URLs: ...     
       http://127.0.0.1:8889/?token=TOKEN

where `TOKEN` is a long hexidecimal string.  

If you paste this last URL into a browser (with the `TOKEN` supplied)
it should take you to a web page displaying the top level directory.  

Navigate to `riemann_book` directory.
Then you can start with `Index.ipynb`, or click on any notebook to launch.

To close the notebook server, you can press Ctrl-C in the container bash shell. 

Then to exit the container:

    jovyan $ exit

See http://jupyter.org/ for more documentation on Jupyter.

### Connecting with a second bash shell

If you have the notebook server running and also want another window open with a bash shell, in another shell on your laptop you can do:

    $ docker exec -it rbook_container bash

### Updating the riemann_book files

In case the `riemann_book` repository changed since the  docker image was built, you could do:

    jovyan $ cd $HOME/riemann_book
    jovyan $ git pull
    
### Updating `clawpack/riemann`

*This should not be necessary with the latest version.*

You may need some Riemann solvers not in the most recent release of Clawpack.  These can be obtained by checking out the master branch (and pulling any changes since you built the image, if necessary):

    jovyan $ cd $HOME/clawpack/clawpack-5.6.1/riemann
    jovyan $ git checkout master
    jovyan $ git pull

If this brings down new Riemann solvers, you will need to compile them and re-install clawpack:

    jovyan $ cd $HOME/clawpack/clawpack-5.6.1
    jovyan $ pip2 install -e .
    
    
### Restarting a container

You can restart the container via::

    $ docker start -a -i rbook_container

The external port should still work for serving notebooks.

### Removing a container

This gets rid of the container and any data that you might have created when running this container:

    $ docker rm rbook_container
    
### Removing the image

Do this if you don't plan to use the image again and want to clean up:

    $ docker rmi clawpack/rbook
    
### Creating a docker image:

The docker image that is available on dockerhub as `clawpack/rbook` was built with this command:

    $ docker build -t clawpack/rbook -f Dockerfile .

If you want to create your own docker image that can run the notebooks, and perhaps also includes some of your own, for example, you could modify `Dockerfile` as desired to `myDockerfile` and then do:

    $ docker build -t my_rbook_image -f myDockerfile .

Note the last `.` on this line!


### More resources:

 - [Docker for Clawpack](http://www.clawpack.org/docker_image.html#docker-image).  The Dockerfile provided here includes everything in the Clawpack-5.7.0 image.
 
 - [Docker documentation](https://docs.docker.com/)
