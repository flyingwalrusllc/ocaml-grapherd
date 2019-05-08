From ocaml/opam2:alpine

Run sudo apk update && \
        sudo apk add m4 && \
        sudo apk add mariadb mariadb-client && \
        sudo apk add mariadb-dev && \
        sudo apk add linux-headers && \
        sudo apk add emacs-nox

Run cd /home/opam/opam-repository && \
        git pull && \
        opam update && \
        opam upgrade && \
        opam install merlin tuareg core async ppx_inline_test ppx_deriving mysql ppx_mysql ppx_mysql_identity httpaf httpaf-async

Run sudo mkdir -p /run/mysqld && \
        sudo chown -R mysql:mysql /run/mysqld && \
        sudo mkdir -p /var/lib/mysql && \
        sudo chown -R mysql:mysql /var/lib/mysql && \
        sudo -u mysql mysql_install_db --datadir=/var/lib/mysql
