#!/bin/bash
for filename in **/*.dot; do
    filename_without_ext="${filename%.*}"
    dot $filename -Tps -o $filename_without_ext.ps
done
