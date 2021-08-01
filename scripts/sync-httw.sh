#!/usr//bin/bash

set -xe

HERE=$(pwd)
REMARKDIR=~/remarkable
FINALDIR=$HERE/images

RAWDIR=/tmp/raw-sync

rm -rf $RAWDIR || echo ""
mkdir $RAWDIR


rmfuse $REMARKDIR &
sleep 3s
echo raw > $REMARKDIR/.mode
rm -rf $RAWDIR/*
cp -Rvu $REMARKDIR/Httw/* $RAWDIR/
pkill rmfuse
umount $REMARKDIR


cd $RAWDIR

for FILE in *.zip; do
  NOTEBOOK=$(basename -s .zip $FILE)
  yes A | unzip -j $NOTEBOOK

  for PAGEFILE in *.rm; do
    PAGE=$(basename -s .rm $PAGEFILE)
    FINAL=$NOTEBOOK-$PAGE

    lines-are-rusty -o $PAGE.svg $PAGE.rm

    inkscape $PAGE.svg -d 300 -o /tmp/out.png
    convert /tmp/out.png -resize "35%" $FINALDIR/$FINAL.png
  done
done

