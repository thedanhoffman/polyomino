#!/usr/bin/env python
"""
A state consists of ...

"""

import sys,copy


# PolyominoGridEdge class--
#	A PolyominoGridEdge object represents the region occupancy and connectivity
#	of a semi-ragged edge.  Semi-ragged means that some number C of leftmost
#	columns have an extra cell, one row lower than the rightmost cells.
#
#	For example, here's a semi-ragged edge with C=5.
#
#		+   +   +   +   +   +                
#		| A   A   A | B | C                     col:      5
#		+   +   +   +   +   +   +   +   +   +   regions:  AAABCCCDC
#		|                     C   C | D | C |   sizeOf:   A:7 B:7 C:8 D:1
#		+   +   +   +   +   +   +   +   +   +   adjacent: AB BC CD
#
#	Without the connectivity information, many edges would appear the same.
#	The connectivity info is necessary to differentiate those states for
#	further expansion.  However, even with connectivity info, an edge can
#	encapsulate multiple histories (and this is desirable).  For the example
#	above, we could have any of the histories below (and others).
#
#		+   +   +   +   +   +
#		| A   A   A | B | C
#		+   +---+   +   +   +   +   +   +   +
#		|   |   |   |   |     C   C | D | C |
#		+   +   +   +   +---+---+   +---+   +
#		|   |   |   |           |           |
#		+---+   +---+   +---+   +---+---+---+
#		|           |   |   |   |           |
#		+   +   +   +---+   +---+   +   +   +
#		
#		                                          +   +   +   +   +   +
#		+   +   +   +   +   +                     | A   A   A | B | C
#		| A   A   A | B | C                       +   +---+---+   +   +   +   +   +   +
#		+   +   +   +   +   +   +   +   +   +     |   |       |   |     C   C | D | C |
#		|           |   |     C   C | D | C |     +   +   +   +   +---+---+   +---+   +
#		+   +---+---+   +---+---+   +---+   +     |   |       |   |       |           |
#		|   |                   |           |     +   +   +   +   +   +   +---+---+---+
#		+---+---+---+---+---+---+---+---+---+     |   |       |   |                   |
#	                                              +   +   +   +   +   +   +   +   +   +
#	                                              |   |       |   |                   |
#		+   +   +   +   +   +                     +---+   +   +   +   +   +   +   +   +
#		| A   A   A | B | C                       |           |   |                   |
#		+   +   +---+   +   +   +   +   +   +     +   +   +   +   +   +   +   +   +   +
#		|       |       |     C   C | D | C |     |           |   |                   |
#		+   +   +   +   +---+---+   +---+   +     +   +   +   +---+   +   +   +   +   +
#		|       |               |           |       ...  ...  ...  ...  ...  ...  ...
#		+---+---+---+---+---+---+---+---+---+

class PolyominoGridEdge(object):

	numColumns = None
	minPiece   = None
	maxPiece   = None
	stampBits  = None
	pieceBits  = None
	debug      = []

	def set_space(numColumns,maxPiece,minPiece=None,debug=None):
		assert (minPiece == None) or (minPiece >= 2), \
		       "PolyominoGridEdge doesn't work for minPiece<2"
		assert (minPiece == None) or (minPiece <= maxPiece), \
		       "PolyominoGridEdge requires minPiece<=maxPiece (%d>%d)" % (minPiece,MaxPIece)
		assert (1 <= numColumns <= 25), \
		       "%d columns out of range for PolyominoGridEdge" % numColumns
		assert (numColumns <= 25), \
		       "%d cells in a piece too large for PolyominoGridEdge" % maxPiece

		PolyominoGridEdge.numColumns = numColumns
		PolyominoGridEdge.maxPiece   = maxPiece
		if (minPiece == None): PolyominoGridEdge.minPiece = maxPiece
		else:                  PolyominoGridEdge.minPiece = minPiece

		if (debug != None):    PolyominoGridEdge.debug    = debug

		if   (numColumns < 4):  PolyominoGridEdge.stampBits = 2
		elif (numColumns < 8):  PolyominoGridEdge.stampBits = 3
		elif (numColumns < 16): PolyominoGridEdge.stampBits = 4
		else:                   PolyominoGridEdge.stampBits = 5

		if   (maxPiece   < 4):  PolyominoGridEdge.pieceBits = 2
		elif (maxPiece   < 8):  PolyominoGridEdge.pieceBits = 3
		elif (maxPiece   < 16): PolyominoGridEdge.pieceBits = 4
		else:                   PolyominoGridEdge.pieceBits = 5

	set_space = staticmethod (set_space)


	def __init__(self,col,regions,sizeOf,adjacent,normalize=False):
		"""
		col is in the range 0..N-1 (N=numColumns) and indicates how many
		    columns at the left contain an extra cell
		regions is a list of N single letters, indicating the region occupying
		    the bottom cell in that column
		sizeOf is a hash from region to the number of cells in that region
		adjacent is a hash of region pairs, indicating regions that are
		    adjacent;  if contains each pair in both ways (e.g. "AB" and BA");
		    what the hash maps TO is unimportant;  pairs in which either piece
		    is full (has size equal to maxPiece) can be left out of the hash
		"""

		assert (PolyominoGridEdge.maxPiece != None), \
		       "PolyominoGridEdge class variables were not initialized"

		self.col      = col
		self.regions  = regions
		self.sizeOf   = sizeOf
		self.adjacent = adjacent
		self.stamped  = False
		if (normalize): self.normalize()


	def expand(self,withWalls=False,asStamps=False):
		if ("expansion" in PolyominoGridEdge.debug):
			print >>sys.stderr, "====="
			print >>sys.stderr, "         %s" % self
		if ("expansionlog" in PolyominoGridEdge.debug):
			selfLog = "%s" % self

		# determine the possible wall configurations for the new cell;  note
		# that we call the south and west neighbors "s" and "w"

		if (self.col == 0):
			sIx = self.col
			s   = self.regions[sIx]
			w   = None
		else:
			sIx = self.col
			wIx = self.col-1
			s   = self.regions[sIx]
			w   = self.regions[wIx]

		sSize = self.sizeOf[s]

		if (self.col == 0):                   # we're at the west end
			if ("walls" in PolyominoGridEdge.debug):
				print >>sys.stderr, "(west end)"
			if (sSize == PolyominoGridEdge.maxPiece):
				walls = ["|-"]                # (s can't grow)
				if ("walls" in PolyominoGridEdge.debug):
					print >>sys.stderr, "(S=%s is full)" % s
			elif (sSize < PolyominoGridEdge.minPiece) and (self.trapped(sIx,s)):
				walls = ["|."]                # (s must grow)
				if ("walls" in PolyominoGridEdge.debug):
					print >>sys.stderr, "(S=%s is trapped)" % s
			else:
				walls = ["|-","|."]           # (s may grow)
		elif (s == w):                        # (new cell must connect both/neither)
			if ("walls" in PolyominoGridEdge.debug):
				print >>sys.stderr, "(S and W same)"
			if (sSize == PolyominoGridEdge.maxPiece):
				walls = ["|-"]                # (can't grow)
				if ("walls" in PolyominoGridEdge.debug):
					print >>sys.stderr, "(S/W=%s is full)" % s
			else:
				walls = ["|-",".."]
		else:
			if ("walls" in PolyominoGridEdge.debug):
				print >>sys.stderr, "(general case)"
			wSize = self.sizeOf[w]
			if (wSize == PolyominoGridEdge.maxPiece):
				wWalls = ["|"]
				if ("walls" in PolyominoGridEdge.debug):
					print >>sys.stderr, "(W=%s is full)" % w
			else:
				wWalls = ["|","."]

			if (sSize == PolyominoGridEdge.maxPiece):
				nWalls = ["-"]
				if ("walls" in PolyominoGridEdge.debug):
					print >>sys.stderr, "(S=%s is full)" % s
			elif (sSize < PolyominoGridEdge.minPiece) and (self.trapped(sIx,s)):
				nWalls = ["."]
				if ("walls" in PolyominoGridEdge.debug):
					print >>sys.stderr, "(S=%s is trapped)" % s
			else:
				nWalls = ["-","."]

			cantMerge = (w+s in self.adjacent) or (sSize+wSize >= PolyominoGridEdge.maxPiece)
			walls = []
			for wWall in wWalls:
				for nWall in nWalls:
					wall = wWall + nWall
					if (cantMerge) and (wall == ".."): continue
					walls += [wall]
			if ("walls" in PolyominoGridEdge.debug):
				print >>sys.stderr, "(wWalls=\"%s\"  nWalls=\"%s\"  walls=[%s])" \
					% (wWalls,nWalls,",".join(["\"%s\""%wall for wall in walls]))

		# for each possible wall configuration, determine the new state

		states  = []
		nextCol = (self.col+1) % len(self.regions)

		if ("|-" in walls):                   #  W closed  S closed
			assert (sSize >= PolyominoGridEdge.minPiece) or (not self.trapped(sIx,s))

			regions  = copy.copy(self.regions)
			sizeOf   = self.sizeOf.copy()
			adjacent = self.adjacent.copy()

			nActive = (not self.trapped(sIx,s))
			if (not nActive): # remove region s
				del sizeOf[s]
				pairs = [pair for pair in adjacent if (s in pair)]
				for pair in pairs:
					del adjacent[pair]

			regions[self.col] = rgn = self.unused_symbol()
			sizeOf[rgn] = 1
			if (w != None):
				adjacent[rgn+w] = True
				adjacent[w+rgn] = True
			if (nActive):
				adjacent[rgn+s] = True
				adjacent[s+rgn] = True

			newState =  PolyominoGridEdge(nextCol,regions,sizeOf,adjacent,normalize=True)
			states   += [(3,newState)]
			if ("expansion" in PolyominoGridEdge.debug):
				print >>sys.stderr, "[|-] --> %s" % newState
			if ("expansionlog" in PolyominoGridEdge.debug):
				print >>sys.stderr, "\t%-44s [|-] --> %s" % (selfLog,newState)

		if (".-" in walls):                   #  W open    S closed
			assert (self.sizeOf[w] < PolyominoGridEdge.maxPiece)
			assert (sSize >= PolyominoGridEdge.minPiece) or (not self.trapped(sIx,s))

			regions  = copy.copy(self.regions)
			sizeOf   = self.sizeOf.copy()
			adjacent = self.adjacent.copy()

			nActive = (not self.trapped(sIx,s))
			if (not nActive): # remove region s
				del sizeOf[s]
				pairs = [pair for pair in adjacent if (s in pair)]
				for pair in pairs:
					del adjacent[pair]

			regions[self.col] = w
			sizeOf [w] += 1
			if (nActive):
				adjacent[w+s] = True
				adjacent[s+w] = True

			newState =  PolyominoGridEdge(nextCol,regions,sizeOf,adjacent,normalize=True)
			states   += [(1,newState)]
			if ("expansion" in PolyominoGridEdge.debug):
				print >>sys.stderr, "[.-] --> %s" % newState
			if ("expansionlog" in PolyominoGridEdge.debug):
				print >>sys.stderr, "\t%-44s [.-] --> %s" % (selfLog,newState)

		if ("|." in walls):                   #  W closed  S open
			assert (sSize < PolyominoGridEdge.maxPiece)

			regions  = copy.copy(self.regions)
			sizeOf   = self.sizeOf.copy()
			adjacent = self.adjacent.copy()

			sizeOf[s] += 1
			if (w != None):
				adjacent[w+s] = True
				adjacent[s+w] = True

			newState =  PolyominoGridEdge(nextCol,regions,sizeOf,adjacent,normalize=True)
			states   += [(2,newState)]
			if ("expansion" in PolyominoGridEdge.debug):
				print >>sys.stderr, "[|.] --> %s" % newState
			if ("expansionlog" in PolyominoGridEdge.debug):
				print >>sys.stderr, "\t%-44s [|.] --> %s" % (selfLog,newState)

		if (".." in walls):                   #  W open    S open
			assert (sSize < PolyominoGridEdge.maxPiece) and (w+s not in self.adjacent)
			assert (s == w) or (sSize+self.sizeOf[w] < PolyominoGridEdge.maxPiece)

			regions  = copy.copy(self.regions)
			sizeOf   = self.sizeOf.copy()
			adjacent = self.adjacent.copy()

			if (s == w):
				sizeOf[s] += 1
			else: # merge region w and s;  change all s's to w's
				sizeOf[w] += sSize + 1
				for (ix,rgn) in enumerate(regions):
					if (rgn == s): regions[ix] = w
				del sizeOf[s]
				pairs = [pair for pair in adjacent if (s in pair)]
				for pair in pairs:
					del adjacent[pair]
					if (pair[0] == s): adjacent[w+pair[1]] = True
					if (pair[1] == s): adjacent[pair[0]+w] = True

			newState =  PolyominoGridEdge(nextCol,regions,sizeOf,adjacent,normalize=True)
			states   += [(0,newState)]
			if ("expansion" in PolyominoGridEdge.debug):
				print >>sys.stderr, "[..] --> %s" % newState
			if ("expansionlog" in PolyominoGridEdge.debug):
				print >>sys.stderr, "\t%-44s [..] --> %s" % (selfLog,newState)

		# convert to the caller's desired list

		if (not withWalls) and (asStamps):
			states = [state.stamp()         for (walls,state) in states]
		elif (not withWalls) and (not asStamps):
			states = [state                 for (walls,state) in states]
		elif (withWalls) and (asStamps):
			states = [(walls,state.stamp()) for (walls,state) in states]

		return states


	def __str__(self):
		rgns = [rgn for rgn in self.sizeOf]
		rgns.sort()
		pairs = [pair for pair in self.adjacent if (pair[0]<pair[1])]
		pairs.sort()
		if (self.col == 0):
			regions = "".join(self.regions) + " "
		else:
			regions = "%s/%s" \
			        % ("".join(self.regions[:self.col]),
			           "".join(self.regions[self.col:]))
		w = (3*PolyominoGridEdge.numColumns) - 1
		return "%s  %-*s  %s" \
			 % (regions,
				w,",".join(["%s%d"%(rgn,self.sizeOf[rgn]) for rgn in rgns]),
				",".join(pairs))


	def __len__(self):
		return len(self.regions)


	def __hash__(self):
		return hash(self.stamp())


	def normalize(self):
		# figure out the attributes with region symbols ordered left-to-right

		(regions,rgnMap) = self.forward_regions()
		sizeOf   = self.map_sizes(rgnMap)
		adjacent = self.map_adjacencies(rgnMap,sizeOf)

		self.regions  = regions
		self.sizeOf   = sizeOf
		self.adjacent = adjacent

		# if we're a full row (not a semi-ragged partial row), see if we should
		# prefer the # left-right-flipped state

		if (self.col == 0) and (self.flippage() < 0):
			(regions,rgnMap) = self.backward_regions()
			sizeOf   = self.map_sizes(rgnMap)
			adjacent = self.map_adjacencies(rgnMap,sizeOf)

			self.regions  = regions
			self.sizeOf   = sizeOf
			self.adjacent = adjacent

		# update other state info, if needed

		if (self.stamped): self.be_stamped()


	def flipped(self):
		if (self.col != 0): raise ValueError

		(regions,rgnMap) = self.backward_regions()
		sizeOf   = self.map_sizes(rgnMap)
		adjacent = self.map_adjacencies(rgnMap,sizeOf)

		return PolyominoGridEdge(0,regions,sizeOf,adjacent)


	def flippage(self):
		"""
		assumes region symbols ordered left-to-right

		returns positive: state is non-symmetric, preferred to its flip-state
		returns zero:     state is symmetric
		returns negative: state is non-symmetric, its flip-state is preferred
		"""

		if (self.col != 0): raise ValueError

		# compare regions to flipped regions;  we prefer the lexicographically
		# lower string

		(revRegions,revRgnMap) = self.backward_regions()
		if   (self.regions < revRegions): return  1
		elif (self.regions > revRegions): return -1

		# compare sizes to those of flipped regions;  we prefer larger regions
		# on the left

		rgns = [rgn for rgn in revRgnMap]
		rgns.sort()

		sizes    = [self.sizeOf[          rgn ] for rgn in rgns]
		revSizes = [self.sizeOf[revRgnMap[rgn]] for rgn in rgns]

		if   (sizes > revSizes): return  1
		elif (sizes < revSizes): return -1

		# compare adjacency list to those of flipped regions;  we prefer
		# adjacencies toward the left

		revSizeOf   = self.map_sizes(revRgnMap)
		revAdjacent = self.map_adjacencies(revRgnMap,revSizeOf)
		pairs    = [pair for pair in self.adjacent if (pair[0]<pair[1])]
		revPairs = [pair for pair in revAdjacent   if (pair[0]<pair[1])]
		pairs.sort()
		revPairs.sort()

		if   (pairs < revPairs): return  1
		elif (pairs > revPairs): return -1

		return 0 # the state must be symmetric


	def forward_regions(self):
		num = 0
		rgnMap = {}
		for rgn in self.regions:
			if (rgn in rgnMap): continue
			rgnMap[rgn] = chr(ord("A")+num)
			num += 1

		regions = [rgnMap[rgn] for rgn in self.regions]
		return (regions,rgnMap)

	def backward_regions(self):
		num = 0
		rgnMap = {}
		for ix in xrange(len(self.regions)-1,-1,-1):
			rgn = self.regions[ix]
			if (rgn in rgnMap): continue
			rgnMap[rgn] = chr(ord("A")+num)
			num += 1

		regions = [rgnMap[rgn] for rgn in self.regions]
		regions.reverse()
		return (regions,rgnMap)


	def map_sizes(self,rgnMap):
		sizeOf = {}
		for rgn in self.sizeOf:
			sizeOf[rgnMap[rgn]] = self.sizeOf[rgn]
		return sizeOf


	def map_adjacencies(self,rgnMap,sizeOf=None):
		if (sizeOf == None): sizeOf = self.sizeOf
		adjacent = {}
		for pair in self.adjacent:
			rgn1 = rgnMap[pair[0]]
			rgn2 = rgnMap[pair[1]]
			if (sizeOf[rgn1] == PolyominoGridEdge.maxPiece) or (sizeOf[rgn2] == PolyominoGridEdge.maxPiece):
				continue # adjacency doesn't matter if region cannot grow
			adjacent[rgn1+rgn2] = True
		return adjacent


	def is_closed(self):
		for rgn in self.sizeOf:
			if (self.sizeOf[rgn] < PolyominoGridEdge.minPiece): return False
		return True


	def trapped(self,rgnIx,rgn):
		for (ix,s) in enumerate(self.regions):
			if (ix!=rgnIx) and (s==rgn): return False
		return True


	def unused_symbol(self):
		for num in xrange(25,-1,-1):
			rgn = chr(ord("A")+num)
			if (rgn not in self.regions): return rgn
		assert (False), "internal error, ran out of region symbols"


	def stamp(self):
		if (not self.stamped): self.be_stamped()
		return self.stampVal


	def be_stamped(self):
		rgns = []
		for rgn in self.regions:
			if (rgn not in rgns): rgns += [rgn]
		rgns.sort()

		shortRegions = [rgn for rgn in self.sizeOf if (self.sizeOf[rgn]) < PolyominoGridEdge.maxPiece]
		shortRegions.sort()

		b = PolyominoGridEdge.stampBits
		p = PolyominoGridEdge.pieceBits
		stamp = 0

		# encode adjacencies
		for ix in xrange(len(shortRegions)-2,-1,-1):
			rgn1 = shortRegions[ix]
			for iy in xrange(len(shortRegions)-1,ix,-1):
				rgn2 = shortRegions[iy]
				stamp <<= 1
				if (rgn1+rgn2 in self.adjacent): stamp += 1

		# encode sizes
		for ix in xrange(len(rgns)-1,-1,-1):
			stamp = (stamp << p) + self.sizeOf[rgns[ix]]

		# encode regions
		for ix in xrange(PolyominoGridEdge.numColumns-1,0,-1): # we omit regions[0]
			stamp = (stamp << b) + ord(self.regions[ix]) - ord("A")

		# encode col
		stamp = (stamp << b) + self.col

		self.stampVal = stamp


	# non-method
	def from_stamp(stamp):
		"""
		stamp is as would be returned from PolyominoGridEdge.stamp()
		"""

		b  = PolyominoGridEdge.stampBits
		p  = PolyominoGridEdge.pieceBits
		bm = (1<<b)-1
		pm = (1<<p)-1

		# decode col
		col   =   stamp & bm
		stamp >>= b

		# decode regions
		regions = ["A"] * PolyominoGridEdge.numColumns
		for ix in xrange(1,PolyominoGridEdge.numColumns):
			num   =   stamp & bm
			stamp >>= b
			regions[ix] = chr(ord("A")+num)

		# decode sizes
		rgns = []
		for rgn in regions:
			if (rgn not in rgns): rgns += [rgn]
		rgns.sort()

		sizeOf = {}
		for num in xrange(len(rgns)):
			size  =   stamp & pm
			stamp >>= p
			rgn = chr(ord("A")+num)
			sizeOf[rgn] = size

		# decode adjacencies
		shortRegions = [rgn for rgn in sizeOf if (sizeOf[rgn]) < PolyominoGridEdge.maxPiece]
		shortRegions.sort()

		adjacent = {}
		for ix in xrange(len(shortRegions)-1):
			rgn1 = shortRegions[ix]
			for iy in xrange(ix+1,len(shortRegions)):
				rgn2 = shortRegions[iy]
				flag  =   stamp & 1
				stamp >>= 1
				if (flag == 1):
					adjacent[rgn1+rgn2] = True
					adjacent[rgn2+rgn1] = True

		return PolyominoGridEdge(col,regions,sizeOf,adjacent)

	from_stamp = staticmethod (from_stamp)

	# non-method
	def from_string(s,normalize=False):
		"""
		s looks like this: ABAACDE / A7,B1,C3,D3,E1 / AB,AC,CD,DE
		The third field (and slash) can be missing if it would be empty
		"""

		fields = s.split("/",2)
		if (len(fields) < 2): raise ValueError
		if (len(fields) == 2): fields += [""]
		(regions,sizes,pairs) = fields

		regions = regions.strip()
		sizes   = sizes.strip().split(",")

		pairs = pairs.strip()
		if (pairs == ""): pairs = []
		else:             pairs = pairs.split(",")

		# convert/validate the regions

		if ("/" not in regions):
			col = 0
		else:
			col = regions.index("/")
			regions = regions[:col] + regions[col+1:]

		if (not regions.isalpha()): raise ValueError

		assert (len(regions) == PolyominoGridEdge.numColumns)

		regions = [rgn for rgn in regions]

		# convert/validate the sizes

		rgns = {}
		for rgn in regions:
			rgns[rgn] = True

		sizeOf = {}
		for item in sizes:
			item = item.strip()
			rgn  = item[0]
			size = int(item[1:])
			if (rgn not in rgns): raise ValueError
			if (rgn in sizeOf): raise ValueError
			if (not 1 <= size <= PolyominoGridEdge.maxPiece): raise ValueError
			sizeOf[rgn] = size

		for rgn in rgns:
			if (rgn not in sizeOf): raise ValueError

		# convert/validate the adjacencies (this is tricky, because we can't
		# determine whether or not some states are impossible without a lot
		# more work)

		adjacent = {}
		for pair in pairs:
			pair = pair.strip()
			rgn1 = pair[0]
			rgn2 = pair[1]
			if (rgn1 not in rgns): raise ValueError
			if (rgn2 not in rgns): raise ValueError
			if (rgn1+rgn2 in adjacent): raise ValueError
			adjacent[rgn1+rgn2] = True
			adjacent[rgn2+rgn1] = True

		return PolyominoGridEdge(col,regions,sizeOf,adjacent,normalize=normalize)

	from_string = staticmethod (from_string)


# initial_states--
#	Generate all possible initial edge states for the top row of the grid.
#	Since there is no history above the top row, we can create these states by
#	considering all 2^(N-1) possibilites for the vertical walls in the row
#	(N=numColumns).
#
# Returns a hash from states (as stamps) to the number of ways that state can
# occur on the top edge.  The number of ways is 1 or 2, the latter owing to
# symmetry.

def initial_states(numColumns,maxPiece):
	numWalls = numColumns-1

	stateToWays = {}
	for wallBits in xrange(1 << numWalls):
		revBits = reverse(wallBits,numWalls)
		if (revBits >  wallBits): continue
		if (revBits == wallBits): ways = 1
		else:                     ways = 2

		regions  = [""] * numColumns
		sizeOf   = {}
		adjacent = {}

		num = 0
		regions[0] = rgn = chr(ord("A")+num)
		sizeOf[rgn] = 1
		regionTooBig = False
		for col in xrange(1,numColumns):
			if (wallBits & 1 == 0):
				regions[col] = rgn
				sizeOf[rgn] += 1
				if (sizeOf[rgn] > maxPiece):
					regionTooBig = True
					break
			else:
				w = rgn
				num += 1
				regions[col] = rgn = chr(ord("A")+num)
				sizeOf[rgn] = 1
				adjacent[rgn+w] = True
				adjacent[w+rgn] = True
			wallBits >>= 1

		if (regionTooBig): continue

		state = PolyominoGridEdge(0,regions,sizeOf,adjacent)
		assert (state not in stateToWays)
		stateToWays[state.stamp()] = ways

	return stateToWays


def reverse(bits,numBits):
	return sum([1<<(numBits-1-i) for i in xrange(numBits) if (bits&(1<<i)!=0)])


# read_edges--
#	Read edges and counts from a text file.  A typical file looks something
#	like this:
#
#		# comment
#		AAAAA  A5      
#		AAAAB  A5,B5     
#		AAABB  A5,B5     
#		AAAAB  A4,B1           AB
#		AAABB  A3,B2           AB
#		AABCB  A4,B5,C1    
#		 ...
#		ABCDE  A3,B1,C3,D1,E2  AB,AC,BC,CD,CE,DE
#		ABCDE  A2,B1,C4,D1,E2  AB,AC,BC,CD,CE,DE
#		ABCDE  A3,B1,C2,D1,E3  AB,AC,BC,CD,CE,DE

def read_edges(f,normalize=True,withCounts=False):

	lineNumber = 0
	for line in f:
		lineNumber += 1
		line = line.strip()
		if (line.startswith("#")): continue

		fields = line.split()

		if (withCounts):
			assert (3 <= len(fields) <= 4), \
			       "wrong number of fields at line %d\n%s" % (lineNumber,line)
			try:
				counts = int(fields[0])
				fields = fields[1:]
			except ValueError:
				assert (False), "can't understand line %d\n%s" % (lineNumber,line)
		else:
			assert (2 <= len(fields) <= 3), \
			       "wrong number of fields at line %d\n%s" % (lineNumber,line)

		edge = PolyominoGridEdge.from_string("/".join(fields),normalize=normalize)
		if (withCounts): yield (counts,edge)
		else:            yield  edge 

