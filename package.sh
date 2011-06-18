#!/bin/bash
rm ./src/*\~
make clean
make
mkdir -p submission/src
mkdir -p submission_echo/src
cp -af ./proto submission/run
cp -af ./echobot submission_echo/run
cp README submission/
cp README_echo submission_echo/
cp ./src/proto.scm ./submission/src/
cp ./src/echobot.ss ./submission_echo/src/
cp install ./submission/
cp install ./submission_echo/
cd submission
tar -cf submission.tar ./*
cd ..
cd submission_echo
tar -cf submission.tar ./*
cd ..


