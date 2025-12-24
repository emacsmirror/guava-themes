# Get the name of the last makefile, convert it to a path, and extract the directory.
MAKEFILE_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

all:
# Show value of MAKEFILE_DIR
	$(info $(MAKEFILE_DIR))

# If it doesn't exist, create ~/.emacs.d/elpa directory
	mkdir -p ~/.emacs.d/elpa

# Copy files inside the makefile directory
	rsync -a --delete $(MAKEFILE_DIR) ~/.emacs.d/elpa
