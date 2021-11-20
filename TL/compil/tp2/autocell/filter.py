#!/usr/bin/python3

import sys

keep = True
for l in sys.stdin:
	if keep:
		if "REMOVE" in l:
			keep = False
		else:
			sys.stdout.write(l)
	else:
		if "KEEP" in l:
			keep = True

		
