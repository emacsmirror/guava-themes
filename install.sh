#!/bin/bash

# Get the script directory
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# Show value of SCRIPT_DIR
echo $SCRIPT_DIR

# If it doesn't exist, create ~/.emacs.d/elpa directory
mkdir -p ~/.emacs.d/elpa

# Copy files inside the script directory
rsync -a --delete $SCRIPT_DIR/. ~/.emacs.d/elpa
