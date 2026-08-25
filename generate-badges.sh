#!/bin/bash
for ((i=29; i <= 32; ++i)); do
    for ((j=i+1; j <= 32; ++j)); do
        wget -O "emacs-$i-$j.svg" "https://img.shields.io/badge/GNU_Emacs-$i%20--%20$j-7F5AB6?logo=gnu-emacs&logoColor=fff"
    done
done
