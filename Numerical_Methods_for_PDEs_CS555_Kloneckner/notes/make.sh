#! /bin/bash

set -e

if [[ "$1" = "watch" ]]; then
  git ls-files | entr ./run-org-conversion.sh
else
  ./run-org-conversion.sh
fi

echo "NOTES BUILD SCCESSFULLY COMPLETED"
