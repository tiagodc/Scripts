#!/bin/bash
for X in *.ppm
do
    ppmtogif "$X" > "$X.gif"
done
