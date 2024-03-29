#!/bin/bash

version=$(grep "! Master version for all instances" **/io.f90 | awk '{print $5}' | tr -d '"')

echo $version
