#!/bin/bash

for cool_file in ../../examples/*.cl; do
    ./myparser $cool_file
done