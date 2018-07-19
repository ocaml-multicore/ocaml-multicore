#case $XARCH in
#i386)
#  ./configure
#  make world.opt
#  sudo make install
#  cd testsuite && make all
#  git clone git://github.com/ocaml/camlp4
#  cd camlp4 && ./configure && make && sudo make install
#  git clone git://github.com/ocaml/opam
#  cd opam && ./configure && make lib-ext && make && sudo make install
#  opam init -y -a git://github.com/ocaml/opam-repository
#  opam install -y utop
#  ;;
#*)
#  echo unknown arch
#  exit 1
#  ;;
#esac

PREFIX=~/inst/ocaml

# turn on heap verifier for CI builds
export OCAMLRUNPARAM=v=0,V=1

set -e

case $XARCH in
i386)
  ./configure
  make world
  ;;
x86_64)
    mkdir -p $PREFIX
    ./configure -prefix $PREFIX -with-debug-runtime
    make -j4 world.opt
    make -C testsuite all-enabled
    ;;
*)
  echo unknown arch
  exit 1
  ;;
esac
