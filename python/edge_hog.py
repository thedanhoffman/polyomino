#!/usr/bin/env python
"""
Author: Bob Harris;  Written in early 2010.

Count the number of ways of tiling an NxM grid with polyominoes of as given
size (or size range).

Note that we allow duplicate pieces, and that tilings that are symmetrically
equivalent are counted for each replicate.  For example, below are some of
the ways to tile a 4x3 with pieces of size 2, 3, 4 and 5 (there are 1,020 ways
total).  Note that all four of the tilings in the top row are symmetrically
equivalent, but we count them each, separately.

Counting is performed by a dynamic programming process, building the board one
cell at a time in a lower-left to upper-right raster pattern.  We track all the
possible 'edge states' that the grid can have, and count the number of ways
each state can occur.  This is described in a little more detail in the
polyomino_grid_edge module.

+---+---+---+---+    +---+---+---+---+    +---+---+---+---+    +---+---+---+---+
|           |   |    |   |           |    |   |           |    |           |   |
+---+   +---+   +    +   +---+   +---+    +   +---+---+---+    +---+---+---+   +
|   |   |       |    |       |   |   |    |   |   |       |    |       |   |   |
+   +---+---+---+    +---+---+---+   +    +---+   +---+   +    +   +---+   +---+
|   |           |    |           |   |    |           |   |    |   |           |
+---+---+---+---+    +---+---+---+---+    +---+---+---+---+    +---+---+---+---+

+---+---+---+---+    +---+---+---+---+    +---+---+---+---+    +---+---+---+---+
|       |       |    |               |    |       |   |   |    |       |       |
+   +   +---+---+    +---+---+---+---+    +   +---+   +   +    +---+---+---+---+
|           |   |    |   |   |       |    |   |       |   |    |       |       |
+---+---+---+   +    +   +   +---+---+    +   +   +   +   +    +   +   +---+---+
|       |       |    |   |   |       |    |   |       |   |    |       |       |
+---+---+---+---+    +---+---+---+---+    +---+---+---+---+    +---+---+---+---+

+---+---+---+---+    +---+---+---+---+    +---+---+---+---+    +---+---+---+---+
|       |       |    |           |   |    |       |   |   |    |           |   |
+   +---+---+---+    +---+---+---+   +    +   +---+   +   +    +---+---+---+   +
|   |   |   |   |    |   |       |   |    |   |       |   |    |   |           |
+   +   +   +   +    +   +   +   +---+    +---+---+---+   +    +   +---+---+   +
|   |   |   |   |    |   |           |    |           |   |    |   |       |   |
+---+---+---+---+    +---+---+---+---+    +---+---+---+---+    +---+---+---+---+

+---+---+---+---+    +---+---+---+---+    +---+---+---+---+    +---+---+---+---+
|   |           |    |   |   |   |   |    |               |    |               |
+   +---+---+---+    +   +   +   +   +    +---+---+---+---+    +   +---+---+---+
|   |           |    |   |   |   |   |    |           |   |    |   |       |   |
+   +---+---+---+    +   +   +---+   +    +---+---+---+   +    +---+---+---+   +
|   |           |    |   |       |   |    |       |       |    |           |   |
+---+---+---+---+    +---+---+---+---+    +---+---+---+---+    +---+---+---+---+

"""

import sys,time
from math import floor,ceil
from polyomino_grid_edge import *


def usage(s=None):
	message = """
edge_hog [options]

  <C>x<R>             fill a grid of <C> columns and <R> rows
  --piece=<S>         allow only pieces of size <S1>
  --piece=<S1>..<S2>  allow pieces of size <S1> thru <S2>
  --step=<N>          perform dynamic programming steps <N> at a time
  --step=row          perform dynamic programming steps a whole row at a time
  --cache=<N>         don't cache more than <N> state expansions
  --nocache           don't cache any state expansions
  --backtrack         create backtracking info
  --progress=<N>      report progress as every <N>th state expansion is cached
  --help              print this help message
  --info              print more detailed information.

  Defaults are to fill a 5x5 grid with pieces of size 5.
"""
	if (s == None): sys.exit (message)
	else:           sys.exit (s + "\n" + message)


def usage_info():
	message = """
This program was written by Bob Harris in early 2010.

Program speed is related to the number of columns.  The program runs reasonably
fast for up to 6 columns but slows down for 7 and higher because of an
explosion of the number of edge states.  The range of shape sizes has little
effect on speed.

Memory consumption also grows with the number of columns, and, to a lesser
extent, with the range of piece sizes.  This is because the number of edge
states explodes.  Memory consumption can be reduced by using --step=row and
limiting the number of cached expansion states with --cache or --nocache.  Both
these options slow down running speed.  At around 9 columns the memory needed
is beyond what most contemporary computers have.

A companion program, fledge_hog, is faster and can solve the 9 column problems.
"""
	sys.exit (message)


def main():
	global debug

	# parse arguments

	maxPiece   = None
	minPiece   = None
	numRows    = None
	numColumns = None
	columnStep = 1
	cacheLimit = None
	backtrack  = False
	progress   = None
	debug      = []

	for arg in sys.argv[1:]:
		if (arg == "--help"):
			usage()
		elif (arg == "--info"):
			usage_info()
		elif (arg.startswith("--piece=")):
			val = arg.split("=",1)[1]
			try:
				if (".." not in val):
					maxPiece = int(val)
				else:
					(minPiece,maxPiece) = val.split("..",1)
					minPiece = int(minPiece)
					maxPiece = int(maxPiece)
					if (minPiece > maxPiece): raise ValueError
			except ValueError:
				usage("can't understand %s" % arg)
		elif (arg == "--step=row"):
			columnStep = "row"
		elif (arg.startswith("--step=")):
			val = arg.split("=",1)[1]
			try:
				columnStep = int_with_units(val)
				if (columnStep < 1): raise ValueError
			except ValueError:
				usage("can't understand %s" % arg)
		elif (arg == "--nocache"):
			cacheLimit = 0
		elif (arg.startswith("--cache=")):
			val = arg.split("=",1)[1]
			try:
				cacheLimit = int_with_units(val)
			except ValueError:
				usage("can't understand %s" % arg)
		elif (arg == "--backtrack"):
			backtrack = True
		elif (arg.startswith("--progress=")):
			val = arg.split("=",1)[1]
			try:
				progress =  int(val)
				debug    += ["cache"]
			except ValueError:
				usage("can't understand %s" % arg)
		elif (arg.startswith("--debug=")):
			debug += (arg.split("=",1)[1]).split(",")
		elif (arg.startswith("--")):
			usage("can't understand %s" % arg)
		elif ("x" in arg):
			try:
				(cols,rows) = arg.split("x",1)
				numRows     = int(rows)
				numColumns  = int(cols)
			except ValueError:
				usage("can't understand %s" % arg)
		else:
			usage("can't understand %s" % arg)

	if (maxPiece == None):
		if (numColumns != None): maxPiece = numColumns
		else:                    maxPiece = 5

	if (minPiece   == None): minPiece   = maxPiece
	if (numRows    == None): numRows    = maxPiece
	if (numColumns == None): numColumns = maxPiece

	if   (columnStep == "row"):     columnStep = numColumns
	elif (columnStep > numColumns): columnStep = numColumns

	if ("cache" in debug) and (progress == None):
		progress = 100*1000

	if ("cache" in debug) or ("wavesize" in debug) or ("time" in debug):
		debug += ["info"]

	# perform the search

	startTime = time.time()

	PolyominoGridEdge.set_space(numColumns,maxPiece,minPiece,debug)

	stateToExpansion = [None] * numColumns
	for col in xrange(numColumns):
		stateToExpansion[col] = {}
	numCachedStates = 0
	expansionCount = expansionLength = 0

	wave = initial_states(numColumns,maxPiece)

	for (row,col) in cell_raster(numRows,numColumns,columnStep):
		if ("info" in debug):
			if (col == 0): print >>sys.stderr
			print >>sys.stderr, ("(%d,%d)" % (row,col)),
		if ("cache" in debug):
			print >>sys.stderr, ("/ %d states in cache" % numCachedStates),
		if ("wavesize" in debug):
			print >>sys.stderr, ("/ wave size %d" % len(wave.keys())),
			if (col == 0):
				count = 0
				for state in wave:
					edge = PolyominoGridEdge.from_stamp(state)
					if (edge.is_closed()): count += 1
				print >>sys.stderr, ("/ closed wave size %d" % count),
		if ("time" in debug):
			print >>sys.stderr, ("(%s since start)" % seconds_to_string(time.time()-startTime)),
		if ("info" in debug):
			print >>sys.stderr

		if ("wave" in debug):
			print >>sys.stderr, "===== wave for row %d, col %d =====" % (row,col)
			print_wave(sys.stderr,wave)
		elif ("expansion" in debug) or ("expansionlog" in debug):
			print >>sys.stderr, "===== row %d, col %d =====" % (row,col)

		if (col == 0):
			print "%dx%d full, %d ways (closed)" \
			    % (numColumns,row,count_wave(wave,isClosed=True))

		newWave = {}
		if (backtrack): stateToBacktrack = {}
		for state in wave:
			if (state in stateToExpansion[col]):
				expansions = stateToExpansion[col][state]
			else:
				edge = PolyominoGridEdge.from_stamp(state)
				if (columnStep == 1):
					expansions = edge.expand(asStamps=True,withWalls=backtrack)
				else:
					numSteps = min(columnStep,numColumns-col)
					expansions = iterated_expansion(edge,numSteps,withWalls=backtrack)
					expansionCount  += 1
					expansionLength += len(expansions)
				if (cacheLimit == None) or (numCachedStates < cacheLimit):
					stateToExpansion[col][state] = expansions
					numCachedStates += 1
					if (progress != None) and (numCachedStates % progress == 0):
						print >>sys.stderr, "(%d states)" % numCachedStates

			if (backtrack):
				ways = wave[state]
				for (walls,q) in expansions:
					if (q not in stateToBacktrack): stateToBacktrack[q] = []
					stateToBacktrack[q] += [(ways,walls,state)]
				expansions = [q for (walls,q) in expansions]

			ways = wave[state]
			for state in expansions:
				try:             newWave[state] += ways
				except KeyError: newWave[state] =  ways

		wave = newWave

		if (backtrack):
			for q in stateToBacktrack:
				print PolyominoGridEdge.from_stamp(q)
				for (ways,walls,p) in stateToBacktrack[q]:
					print "  %d,%d,%s" \
					    % (ways,walls,PolyominoGridEdge.from_stamp(p))

		if ("lists" in debug):
			print >>sys.stderr, "%d/%d expansions, average %.1f" \
			                  % (expansionLength,expansionCount,
			                     float(expansionLength)/expansionCount)

	if ("info" in debug):
		print >>sys.stderr
		print >>sys.stderr, ("(%d,%d)" % (numRows,0)),
	if ("cache" in debug):
		print >>sys.stderr, ("/ %d states in cache" % numCachedStates),
	if ("wavesize" in debug):
		print >>sys.stderr, ("/ wave size %d" % len(wave.keys())),
		count = 0
		for state in wave:
			edge = PolyominoGridEdge.from_stamp(state)
			if (edge.is_closed()): count += 1
		print >>sys.stderr, ("/ closed wave size %d" % count),
	if ("time" in debug):
		print >>sys.stderr, ("(%s since start)" % seconds_to_string(time.time()-startTime)),
	if ("info" in debug):
		print >>sys.stderr

	if ("wave" in debug):
		print >>sys.stderr, "===== final wave ====="
		print_wave(sys.stderr,wave)

	print "%dx%d full, %d ways (closed)" \
	    % (numColumns,numRows,count_wave(wave,isClosed=True))
	print "===== closing states ====="
	print_wave(sys.stderr,wave,isClosed=True)
	print "%d ways" % count_wave(wave,isClosed=True)


def iterated_expansion(edge,numSteps,withWalls=False):
	wavelet = [(0,edge)]
	for step in xrange(numSteps):
		newWavelet = []
		for (west,edgelet) in wavelet:
			west <<= 2
			expansions =  edgelet.expand(withWalls=True,asStamps=(step==numSteps-1))
			newWavelet += [(west+walls,state) for (walls,state) in expansions]
		wavelet = newWavelet
	if (not withWalls):
		wavelet = [state for (walls,state) in wavelet]
	return wavelet


def count_wave(wave,isClosed=False):
	ways = 0
	for state in wave:
		edge = PolyominoGridEdge.from_stamp(state)
		if (isClosed) and (not edge.is_closed()): continue
		ways += wave[state]
	return ways


def print_wave(f,wave,isClosed=False):
	states = [state for state in wave]
	states.sort()
	for state in states:
		edge = PolyominoGridEdge.from_stamp(state)
		if (isClosed) and (not edge.is_closed()): continue
		print >>f, "%-3d %s" % (wave[state],edge)


def cell_raster(numRows,numColumns,columnStep=1):
	for row in xrange(1,numRows):
		for col in xrange(0,numColumns,columnStep):
			yield (row,col)


def int_with_units(s):
	if (s.endswith("K")):
		multiplier = 1000
		s = s[:-1]
	elif (s.endswith("M")):
		multiplier = 1000 * 1000
		s = s[:-1]
	elif (s.endswith("G")):
		multiplier = 1000 * 1000 * 1000
		s = s[:-1]
	else:
		multiplier = 1

	try:               return      int(s)   * multiplier
	except ValueError: return ceil(float(s) * multiplier)


def seconds_to_string(seconds):
	minutes =  int(floor(seconds/60))
	seconds -= 60*minutes
	hours   =  minutes/60
	minutes -= 60*hours

	if (hours > 0): return "%dh%02dm%04.1fs" % (hours,minutes,seconds)
	else:           return      "%dm%04.1fs" %       (minutes,seconds)


if __name__ == "__main__": main()
