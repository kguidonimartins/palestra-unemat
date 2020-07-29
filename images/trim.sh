#!/usr/bin/env bash

for file in *.png ; do convert $file -trim trim/${file} ; done
