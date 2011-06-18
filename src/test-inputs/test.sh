#!/bin/bash

for i in *.in
do
  echo $i
  ../proto t < $i
done
