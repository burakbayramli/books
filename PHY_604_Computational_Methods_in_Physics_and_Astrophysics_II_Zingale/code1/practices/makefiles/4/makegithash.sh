#/bin/sh

HASH=`git rev-parse HEAD`

cat > githash.f90 <<EOF
module gitstuff

  implicit none

   integer :: HASHLEN=${#HASH}

contains

  function githash() result (hash)
    implicit none

    character (len=HASHLEN) :: hash
    hash = "${HASH}"

    return
  end function githash
end module gitstuff
EOF
