#!/bin/bash
rm *.csv
rm *.tar.gz
wget https://coinmetrics.io/data/all.tar.gz
tar -xzvf all.tar.gz
