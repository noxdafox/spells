#!/bin/bash
#
# Set up a temporary debian mirror allowing to install packages
# from a local folder.
#
# Usage: debinstall.sh <folder> <deb-package> <deb-package>

FOLDER=$(realpath $1)
SRCLIST=/etc/apt/sources.list.d/debinstall_tmp.list

set -e

# ensure the folder exists
if [ ! -d $FOLDER ]; then
    echo "$FOLDER does not exists"
    exit 1
fi

# set up local mirror in folder
dpkg-scanpackages $FOLDER | gzip > $FOLDER/Packages.gz

# add mirror to sources list
cat <<EOF > $SRCLIST
deb [trusted=yes] file:///${FOLDER#/} ./
EOF

# update package collection
apt-get update

# install requested files
shift
apt-get install -y $@

# remove mirror from sources list
rm $SRCLIST

# update package collection
apt-get update

exit 0
