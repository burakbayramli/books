
FROM clawpack/v5.7.0_dockerimage:release
# this dockerhub image has a user jovyan and clawpack installed 
# in /home/jovyan/clawpack-v5.7.0


ENV NB_USER jovyan
User jovyan

WORKDIR ${HOME}


RUN git clone https://github.com/ipython-contrib/jupyter_contrib_nbextensions.git
RUN pip install ipywidgets
RUN pip install --user -e jupyter_contrib_nbextensions

ENV PATH ${PATH}:/home/jovyan/.local/bin
RUN jupyter contrib nbextension install --user
RUN jupyter nbextension enable widgetsnbextension --py
RUN jupyter nbextension enable equation-numbering/main


# Add book's files
RUN git clone --depth=1 https://github.com/clawpack/riemann_book

RUN pip install --user --no-cache-dir -r $HOME/riemann_book/requirements.txt

# The command below starts the notebook server, but better to not
# do this by default in case the user also wants to examine files or use
# the docker container for running other things...
#CMD jupyter notebook riemann_book/Index.ipynb --ip=0.0.0.0 --port=8889 --no-browser

