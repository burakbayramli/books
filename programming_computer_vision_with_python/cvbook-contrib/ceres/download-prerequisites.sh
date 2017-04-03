#/bin/bash

set -x

fetch() {
  dir=${1}
  url=${2}
  if ! [ -d ${dir} ]; then
    curl -L -O ${url}
    mkdir -p ${dir}
    tar -xj -C ${dir} --strip-components 1 -f $(basename ${url})
    rm $(basename ${url})
  fi
}

EIGEN_URL=http://bitbucket.org/eigen/eigen/get/3.2.0.tar.bz2
fetch eigen $EIGEN_URL

GLOG_URL=https://google-glog.googlecode.com/files/glog-0.3.3.tar.gz
fetch glog $GLOG_URL

GFLAGS_URL=https://gflags.googlecode.com/files/gflags-2.0-no-svn-files.tar.gz
fetch gflags $GFLAGS_URL

SUITESPARSE_URL=\
http://www.cise.ufl.edu/research/sparse/SuiteSparse/current/SuiteSparse.tar.gz
fetch suitesparse $SUITESPARSE_URL

METIS_URL=http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/OLD/metis-4.0.1.tar.gz
fetch suitesparse/metis-4.0 $METIS_URL

# Can use -framework Accelerate for blas / lapack on OS X. Probably not quite
# as fast, but good enough for now.
#GOTOBLAS2_URL=\
#http://www.tacc.utexas.edu/documents/13601/b58aeb8c-9d8d-4ec2-b5f1-5a5843b4d47b
#fetch gotoblas2 $GOTOBLAS2_URL

PROTOBUF_URL=https://protobuf.googlecode.com/files/protobuf-2.5.0.tar.bz2
fetch protobuf $PROTOBUF_URL

CERES_URL=https://ceres-solver.googlecode.com/files/ceres-solver-1.6.0.tar.gz
fetch ceres $CERES_URL
