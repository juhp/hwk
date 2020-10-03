#!/bin/bash

seq 1 10 | hwk 'map ("number " ++)'

seq -100 100 | hwk 'filter (0>) . ints' | hwk 'sum . ints'
