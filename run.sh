#!/bin/bash

echo "compiling..."
make blokus >& /dev/null
if [ $? -eq 0 ]; then
	echo "done"
	clisp -lp src/obj/ -i main -x '(main)'
else
	echo "something fail :("
fi
