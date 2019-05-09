From ocaml/opam2:ubuntu

Run sudo apt-get -y update && \
        sudo apt-get -y upgrade && \
        sudo apt-get -y install m4 && \
        sudo apt-get -y install postgresql-11 postgresql-server-dev-11 && \
        sudo apt-get -y install linux-headers-generic

Run cd /home/opam/opam-repository && \
        git pull && \
        opam update && \
        opam upgrade && \
        opam install core async ppx_inline_test ppx_deriving httpaf httpaf-async

