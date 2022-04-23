//-------+---------+---------+---------+---------+---------+---------+--------=
// Author: Bob Harris;  Written in early 2010.
//
// fledge_hog--
//	Count the number of ways of tiling an NxM grid with polyominoes of as given
//	size (or size range).
//
//		>>>>>  WARNING: counts CAN OVERFLOW and NO OVERFLOW CHECK is  <<<<<
//		>>>>>  performed.  See the information about countBits below  <<<<<
//
// The problem to be solved is specified with COMPILE-TIME controls (there are
// no command-line arguments).  Search down for "control definitions" for
// information about the controls.
//
//----------
//
//	Note that we allow duplicate pieces, and that tilings that are symmetrically
//	equivalent are counted for each replicate.  For example, below are some of
//	the ways to tile a 4x3 with pieces of size 2, 3, 4 and 5 (there are 1,020
//	ways total).  Note that all four of the tilings in the top row are
//	symmetrically equivalent, but we count them each, separately.
//
//	+---+---+---+---+    +---+---+---+---+    +---+---+---+---+    +---+---+---+---+
//	|           |   |    |   |           |    |   |           |    |           |   |
//	+---+   +---+   +    +   +---+   +---+    +   +---+---+---+    +---+---+---+   +
//	|   |   |       |    |       |   |   |    |   |   |       |    |       |   |   |
//	+   +---+---+---+    +---+---+---+   +    +---+   +---+   +    +   +---+   +---+
//	|   |           |    |           |   |    |           |   |    |   |           |
//	+---+---+---+---+    +---+---+---+---+    +---+---+---+---+    +---+---+---+---+
//
//	+---+---+---+---+    +---+---+---+---+    +---+---+---+---+    +---+---+---+---+
//	|       |       |    |               |    |       |   |   |    |       |       |
//	+   +   +---+---+    +---+---+---+---+    +   +---+   +   +    +---+---+---+---+
//	|           |   |    |   |   |       |    |   |       |   |    |       |       |
//	+---+---+---+   +    +   +   +---+---+    +   +   +   +   +    +   +   +---+---+
//	|       |       |    |   |   |       |    |   |       |   |    |       |       |
//	+---+---+---+---+    +---+---+---+---+    +---+---+---+---+    +---+---+---+---+
//
//	+---+---+---+---+    +---+---+---+---+    +---+---+---+---+    +---+---+---+---+
//	|       |       |    |           |   |    |       |   |   |    |           |   |
//	+   +---+---+---+    +---+---+---+   +    +   +---+   +   +    +---+---+---+   +
//	|   |   |   |   |    |   |       |   |    |   |       |   |    |   |           |
//	+   +   +   +   +    +   +   +   +---+    +---+---+---+   +    +   +---+---+   +
//	|   |   |   |   |    |   |           |    |           |   |    |   |       |   |
//	+---+---+---+---+    +---+---+---+---+    +---+---+---+---+    +---+---+---+---+
//
//	+---+---+---+---+    +---+---+---+---+    +---+---+---+---+    +---+---+---+---+
//	|   |           |    |   |   |   |   |    |               |    |               |
//	+   +---+---+---+    +   +   +   +   +    +---+---+---+---+    +   +---+---+---+
//	|   |           |    |   |   |   |   |    |           |   |    |   |       |   |
//	+   +---+---+---+    +   +   +---+   +    +---+---+---+   +    +---+---+---+   +
//	|   |           |    |   |       |   |    |       |       |    |           |   |
//	+---+---+---+---+    +---+---+---+---+    +---+---+---+---+    +---+---+---+---+
//
//	Counting is performed by a dynamic programming process, building the board
//	one cell at a time in a lower-left to upper-right raster pattern.  We track
//	all the possible 'edge states' that the grid can have, and count the number
//	of ways each state can occur.
//
//	An edge state consists of the region occupancy and connectivity of a semi-
//	ragged edge.  Semi-ragged means that some number C of leftmost columns have
//	an extra cell, one row lower than the rightmost cells.
//
//	For example, here's a semi-ragged edge with C=5.
//
//		+   +   +   +   +   +                
//		| A   A   A | B | C                     col:      5
//		+   +   +   +   +   +   +   +   +   +   regions:  AAABCCCDC
//		|                     C   C | D | C |   sizeOf:   A:7 B:7 C:8 D:1
//		+   +   +   +   +   +   +   +   +   +   adjacent: AB BC CD
//
//	Without the connectivity information, many edges would appear the same.
//	The connectivity info is necessary to differentiate those states for
//	further expansion.  However, even with connectivity info, an edge can
//	encapsulate multiple histories (and this is desirable).  For the example
//	above, we could have any of the histories below (and many many others).
//
//		+   +   +   +   +   +
//		| A   A   A | B | C
//		+   +---+   +   +   +   +   +   +   +
//		|   |   |   |   |     C   C | D | C |
//		+   +   +   +   +---+---+   +---+   +
//		|   |   |   |           |           |
//		+---+   +---+   +---+   +---+---+---+
//		|           |   |   |   |           |
//		+   +   +   +---+   +---+   +   +   +
//		  ...  ...  ...  ...  ...  ...  ...
//
//		                                          +   +   +   +   +   +
//		+   +   +   +   +   +                     | A   A   A | B | C
//		| A   A   A | B | C                       +   +---+---+   +   +   +   +   +   +
//		+   +   +   +   +   +   +   +   +   +     |   |       |   |     C   C | D | C |
//		|           |   |     C   C | D | C |     +   +   +   +   +---+---+   +---+   +
//		+   +---+---+   +---+---+   +---+   +     |   |       |   |       |           |
//		|   |                   |           |     +   +   +   +   +   +   +---+---+---+
//		+---+---+---+---+---+---+---+---+---+     |   |       |   |                   |
//	                                              +   +   +   +   +   +   +   +   +   +
//	                                              |   |       |   |                   |
//		+   +   +   +   +   +                     +---+   +   +   +   +   +   +   +   +
//		| A   A   A | B | C                       |           |   |                   |
//		+   +   +---+   +   +   +   +   +   +     +   +   +   +   +   +   +   +   +   +
//		|       |       |     C   C | D | C |     |           |   |                   |
//		+   +   +   +   +---+---+   +---+   +     +   +   +   +---+   +   +   +   +   +
//		|       |               |           |       ...  ...  ...  ...  ...  ...  ...
//		+---+---+---+---+---+---+---+---+---+
//
//----------

#include <stdlib.h>				// standard C stuff
#define  true  1
#define  false 0
#include <stdio.h>				// standard C i/o stuff
#include <string.h>				// standard C string stuff
#include <ctype.h>				// standard C upper/lower stuff
#include <math.h>				// standard C math stuff
#include <time.h>				// standard C time stuff
#include "rand32.h"				// Bob's portable random number stuff

#include <stdint.h>
typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int64_t  s64;

//----------
//
// control definitions (these take the place of command-line arguments)
//
// Program speed is related to the number of columns.  The program is bliningly
// fast for up to 7 columns and runs reasonably fast for 8.  At 9 columns the
// number of edge states explodes, slowing things down considerably.
//
// Memory consumption also grows with the number of columns, and, to a lesser
// extent, with the range of piece sizes.  This is because the number of edge
// states explodes.  Memory consumption can be reduced by setting columnStep to
// match the number of columns.  Even with that, at around 9 columns the memory
// needed is beyond what most contemporary computers have.
//
// The program allocates all needed memory up front.  The main memory object is
// the "wave hash", which maps edge staes to their count in the previous and
// current dynamic programming wave.  The amount of memory allocated is
// controlled by expectedWave.  This make take some trial and error to set
// correctly (if you set it too low, the program will just abort when it runs
// out of memory).  Reasonable values (with columnStep == numColumns) are shown
// in the table below.  The downside of setting the value too high is only
// that it may cause your machine to swap memory to disk, or that allocation
// may just fail.
//
//		numColumns | expectedWave
//		-----------+-------------
//		     4     | 70
//		     5     | 600
//		     6     | 18,000
//		     7     | 125,000
//		     8     | 2,300,000
//		     9     | 50,000,000
//
// Settings for counting the example shown in the header above, counting the
// ways to tile a 4x3 with pieces of size 2, 3, 4 and 5, are:
//
//	numColumns 4
//	numRows    3
//	minPiece   2
//	maxPiece   5
//
// trackAdjacency must be defined as true if there is a possibility that one
// piece can surround another.
//
// separateFulls should be left as false.  The "true" condition represents an
// earlier version of the program, which used slightly more states.
//
// Also beware that counts CAN OVERFLOW and that NO OVERFLOW CHECK is performed.
// The number of bits used for counts in controlled by countBits.  For 64 bits
// or less the counts are a simple integer type.  For more bits the counts are
// a multiple-integer struct.  The differences are managed more or less
// seemlessly by use of macros.  Currently, the largest size supported is 96
// bits.  Be aware that more bits for counts will increase memory needs.
//
//----------

#define numColumns 7				// number of columns in the grid
#define numRows    10				// number of rows in the grid
#define minPiece   7				// smallest piece
#define maxPiece   7				// largest piece (0 means there is no max)

#define columnStep numColumns		// number of columns we'll step through in
									// .. a single expansion

#define expectedWave   3*1000*1000	// number of edge states we'll see
#define countBits      96			// number of bits to use for wave counts
#define trackAdjacency true			// true  => track "deep" adjacency
									// false => don't
#define separateFulls  false		// true  => track full regions separately
									// false => don't
#define poolCacheBytes 0			// number of bytes to allocate to cache
									// .. expansions (can be zero)
#define deallocOnExit  false		// true  => deallocate all memory
									// false => don't bother

// debug settings (true/false)

#define dbgInitialize      false
#define dbgPack            false
#define dbgUnpack          false
#define dbgWaveStats       true
#define dbgWaveHash        false
#define dbgWaveHashDump    false
#define dbgExpansion       false
#define dbgExpansionIter   false
#define dbgExpansionDraw   false
#define dbgExpansionLog    false
#define dbgShowRecurrence  false
#define dbgRecurrenceStats false
#define dbgPoolStats       false
#define dbgWalls           false
#define dbgNoFlippage      false	// disable symmetry folding
#define dbgBigArithmetic   false

#define dbgPropagatedProgress 0		// zero     => no progress report
									// non-zero => progress report every nth

// debug settings (defined/undefined)

#define dbgSanityExpansion			// if defined, we include a sanity check in
									// .. edge expansion

//----------
//
// private global data
//
//----------

#define N numColumns
#define R numRows
#define P maxPiece
#define Q minPiece

#define Nc2 (((N)*((N)-1))/2)

#if (N < 1)
#error ***** numColumns must be at least 1 *****
#endif

#if (R < 1)
#error ***** numRows must be at least 1 *****
#endif

#if (Q < 1)
#error ***** minPiece must be at least 1 *****
#endif

#if ((P != 0) && (P < Q))
#error ***** maxPiece must be at least as big as minPiece *****
#endif

#if (columnStep < 1)
#error ***** columnStep must be at least 1 *****
#endif

#if (columnStep > N)
#error ***** columnStep can't be bigger than numColumns *****
#endif

#if ((!trackAdjacency) && ((P == 0) || (P >= 23) || (P-Q >= 8)))
#error ***** trackAdjacency is improperly set *****
#endif

#if ((P == 0) && (separateFulls))
#error ***** separateFulls is improperly set *****
#endif

// type for wave counts

#if (countBits <= 32)				// === counts in 32 bits ===

typedef u32 ucount;
#define ucountFmt  "%u"

#elif (countBits <= 64)				// === counts in 64 bits ===

typedef u64 ucount;
#define ucountFmt  "%ju"

#elif (countBits <= 80)				// === counts in 80 bits ===

typedef struct ucount
	{
	u64	ls;							// least significant 64 bits
	u16	ms;							// most  significant 16 bits
	} ucount;
#define ucountFmt  "%s"

#elif (countBits <= 96)				// === counts in 96 bits ===

typedef struct ucount
	{
	u64	ls;							// least significant 64 bits
	u32	ms;							// most  significant 32 bits
	} ucount;
#define ucountFmt  "%s"

#else
#error ***** countBits can't be bigger than 96 *****
#endif // countBits

#if (countBits <= 64)
#define fmt_ucount(v)     (v)
#define accumulate(sum,v) ((sum)+=(v))
#define to_ucount(v)      (v)
#define is_zero(v)        ((v)==0)
#else
#define fmt_ucount(v)     ucount_to_string(&(v))
#define accumulate(sum,v) ucount_accumulation(&(sum),&(v));
#define to_ucount(v)      val_to_ucount(v)
#define is_zero(v)        ucount_is_zero(&v)
#endif // countBits

// unpacked version of an edge;  note that the extra entries in sizeOf and
// adjacent (the N+1 instead of N), allow us to temporarily have N in the
// regions list

typedef struct unpackedge
	{
	u8	col;						// 0..N-1
	u8	regions [N];				// values in 0..N-1 (sometimes N)
 	u8	sizeOf  [N+1];				// values in 1..P
	u8	adjacent[N+1][N+1];			// true/false
	} unpackedge;

#define noRegion ((u8) -1)

#if (P == 0)
#define is_full(sz) (false)			// sz is a sizeOf value
#else
#define is_full(sz) ((sz) == P)
#endif // P

// packed version of an edge

#define packedgeChunks 3
typedef struct packedge { u32 v[packedgeChunks]; } packedge;

#define empty_packedge(e)   (((e).v[0] == 0)        && ((e).v[1] == 0)        && ((e).v[2] == 0))
#define equal_packedge(a,b) (((a).v[0] == (b).v[0]) && ((a).v[1] == (b).v[1]) && ((a).v[2] == (b).v[2]))

// lists of edges

typedef struct edgelist
	{
	u32			len;				// number of edges in the list
	packedge	e[1];				// (variable-length array of) edges
	} edgelist;

typedef struct edgelist4			// (big enough to hold expansions for a
	{								//  .. single column step)
	u32			len;
	packedge	e[4];
	} edgelist4;

#define maxExpansionsInRow (1<<(2*(N)-1))

typedef struct edgelistrow			// (big enough to hold expansions for a
	{								//  .. full row step)
	u32			len;
	packedge	e[maxExpansionsInRow];
	} edgelistrow;

#define sizeof_edgelist(len) (sizeof(edgelist) + ((len)-1)*sizeof(packedge))

// wave hash
//
// The wave hash maps an edge state to two counts (on for the previous wave,
// the other for the current wave).
//
// $$$ having key as a component in a struct is an artifact of an earlier
// $$$ .. implementation, and should be removed

#define waveHashUtilization .93		// hash efficiency
#define waveHashMaxTries 1000		// number of attempts we'll make to
									// .. relocate a hash entry, before we
									// .. give up

#define waveHashEntries (2+((int)(expectedWave/waveHashUtilization)&(~1)))

typedef struct waveentry
	{
	packedge	key;
	} waveentry;

waveentry* waveHash       = NULL;
ucount*    wavePrevious   = NULL;
ucount*    waveCurrent    = NULL;
#if (poolCacheBytes != 0)
edgelist** waveExpansions = NULL;
char*      expansionsPool = NULL;
char*      nextInPool;
u32        bytesLeftInPool;
u32        entriesInPool;
#endif // poolCacheBytes

u32 entriesInHash;

#define preHashIndex ((u32) -1)
#define noHashIndex  ((u32) -2)

// miscellany

time_t	startTime;					// time the search was started
#define elapsed_time() seconds_to_string(difftime(time(NULL),startTime))

#define edgeFmt    "%s"
#define edgeLogFmt "%-44s"

//----------
//
// prototypes for private functions
//
//----------

int main (int argc, char** argv);

static void      initialize_wave        (void);
static edgelist*  expand_edge            (packedge* e);
static edgelist*  iterated_expansion     (packedge* e, int numSteps);
static int        edge_flippage          (unpackedge* u);
static void       normalize_edge         (unpackedge* u);
static ucount     count_closed_wave      (void);
static int        edge_is_closed         (unpackedge* u);
static void       pack_edge              (unpackedge* u, packedge* e);
static void       unpack_edge            (packedge* e, unpackedge* u);
static void       copy_unpackedge        (unpackedge* src, unpackedge* dsst);
static void       clear_adjacencies      (unpackedge* u);
static u8         number_of_regions      (unpackedge* u);
static void       string_to_packedge     (char* s, packedge* e);
static void       string_to_unpackedge   (char* s, unpackedge* u);
static char*      packedge_to_string     (packedge* e);
static char*      unpackedge_to_string   (unpackedge* u);
static void       draw_packedge          (FILE* f, packedge* e, char* prefix);
static void       draw_unpackedge        (FILE* f, unpackedge* u, char* prefix);
static void       initialize_wave_hash   (void);
static void       print_wave_hash        (FILE* f);
static void       dump_wave              (FILE* f, int showClosed);
static void       add_to_wave_hash       (packedge* e, ucount count);
static ucount     wave_hash_previous     (packedge* e);
static ucount     current_to_previous    (void);
static packedge*  next_in_current_wave   (u32* h, ucount* count);
static packedge*  next_in_previous_wave  (u32* h, ucount* count, int clearAsWeGo);
static u32        hash_func1             (packedge* e);
static u32        hash_func2             (packedge* e, u32 prohibited);
static u32        hash_func3             (packedge* e, u32 prohibited, u32 prohibited2);

#if (poolCacheBytes != 0)
static edgelist*  add_expansions_to_pool (edgelist* expansions);
#endif // poolCacheBytes

static u32        reverse_bits           (int numBits, u32 bits);
static char*      int_to_string          (s64 v);
static char*      bits_string            (int numBits, u64 bits);
static char*      seconds_to_string      (float seconds);

#if (countBits > 64)
static char*      ucount_to_string       (ucount* v);
static void       ucount_accumulation    (ucount* sum, ucount* v);
static ucount     val_to_ucount          (u64 v);
static int        ucount_is_zero         (ucount* v);
#endif // countBits

//----------
//
// fledge_hog--
//  Main program
//
//----------

int main (int argc, char** argv)
	{
	int			row, col, nextCol;
	ucount		waveSize;
	packedge*	e, *ex;
	edgelist*	expansions;
	ucount		ways;
	u32			h;
	int			i, numSteps;
	u32			edgesPropagated, edgesToPropagate;
	u32			edgesExpanded;
	u64			numExpansions;

	if (argc != 1)
		{ fprintf (stderr, "give me no arguments!\n");  exit (1); }

	srand32 (39813);    // set random seed non-randomly
	time (&startTime);

//...
//	{
//	packedge ee;
//	unpackedge uu;
//	ee.v[2] = 0x00000000;
//	ee.v[1] = 0x0000000D;
//	ee.v[0] = 0x20912405;
//	unpack_edge (&ee, &uu);
//	exit(0);
//	}
//...
//	{
//	packedge ee;
//	string_to_packedge ("AABBCCCCD A4,B3,C8,D3 AB,BC,CD", &ee);
//	expansions = iterated_expansion (&ee, numColumns);
//	for (i=0 ; i<expansions->len ; i++)
//		fprintf (stderr, edgeFmt "\n", packedge_to_string(&expansions->e[i]));
//	exit(0);
//	}
//...
//	{
//	packedge ee;
//	unpackedge uu;
//	string_to_packedge ("ABC/ADEE A5,B1,C1,D1,E2 AB,AC,AD,BC,DE", &ee);
//	unpack_edge (&ee, &uu);
//	normalize_edge (&uu);
//	fprintf (stderr, edgeFmt "\n", unpackedge_to_string(&uu));
//	exit(0);
//	}
//...
//	{
//	packedge ee;
//	string_to_packedge ("ABCD/E A1,B1,C1,D1,E5 AB,BC,CD", &ee);
//	initialize_wave_hash ();
//	add_to_wave_hash (&ee, 100);
//	current_to_previous ();
//	h = preHashIndex;
//	while ((e = next_in_previous_wave(&h,NULL,false)) != NULL)
//		fprintf (stderr, edgeFmt "\n", packedge_to_string(e));
//	exit(0);
//	}
//...

	// set up the first set of row states

	initialize_wave ();

	if (dbgWaveHashDump)
		{
		fprintf (stderr, "===== initial wave =====\n");
		dump_wave (stderr, /*showClosed*/ true);
		}
	if (dbgWaveStats)
		fprintf (stderr, "for 1 row, %d of %d entries used (%.1f%%)\n",
						 entriesInHash, waveHashEntries,
						 (entriesInHash*100.0)/waveHashEntries);

	// perform the search

	for (row=1 ; row<R ; row++)
			for (col=0 ; col<N ; col+=columnStep)
		{
		nextCol = col+columnStep;
		if (nextCol > N) nextCol = N;

		//printf ("===== expanding to (%d,%d) =====\n", row, nextCol);
		waveSize = current_to_previous ();
		if (col == 0)
			{
			ways = count_closed_wave ();
			printf ("%dx%d --> " ucountFmt " ways (%s)\n",
			        N, row, fmt_ucount(ways), elapsed_time());
			}

		edgesPropagated  = 0;
		edgesToPropagate = entriesInHash;
		h = preHashIndex;
		if (dbgRecurrenceStats) edgesExpanded = numExpansions = 0;
		while ((e = next_in_previous_wave (&h, &ways, true)) != NULL)
			{
			numSteps = N - col;
			if (numSteps > columnStep) numSteps = columnStep;

			// if we are caching expansions, see if this state was in the
			// cache, and if so, use it

#if (poolCacheBytes != 0)
			expansions = waveExpansions[h];
			if (expansions != NULL) goto apply_expansions;
#endif // poolCacheBytes

			// otherwise, compute the expansion states

			if (numSteps == 1) expansions = expand_edge (e);
			else               expansions = iterated_expansion (e, numSteps);

			// if we are caching expansions, try to add these to the cache
			// note:  we assume that the hash has not been shuffled since we
			//        found e in the hash

#if (poolCacheBytes != 0)
			if (bytesLeftInPool > sizeof_edgelist(expansions->len))
				waveExpansions[h] = add_expansions_to_pool (expansions);
		apply_expansions:
#endif // poolCacheBytes

			// apply the expansions (to compute the recurrence)

			for (i=0 ; i<expansions->len ; i++)
				{
				ex = &expansions->e[i];
				add_to_wave_hash (ex, ways);
				}


			edgesPropagated++;
			if ((dbgPropagatedProgress != 0)
			 && (edgesPropagated % dbgPropagatedProgress == 0))
				fprintf (stderr, "  %d edges propagated (%.1f%%, %s)\n",
				                 edgesPropagated,
				                 (edgesPropagated*100.0) / edgesToPropagate,
				                 elapsed_time());

			if (dbgRecurrenceStats)
				{ edgesExpanded++;  numExpansions += expansions->len; }
			}

		if (dbgWaveHashDump)
			{
			fprintf (stderr, "===== wave hash for (%d,%d) =====\n", row, nextCol);
			dump_wave (stderr, /*showClosed*/ true);
			}
		if (dbgWaveStats)
			{
			if (nextCol == N) fprintf (stderr, "for %d rows", row+1);
			             else fprintf (stderr, "for (%d,%d)", row, nextCol);
			fprintf (stderr, ", %d of %d entries used (%.1f%%)\n",
			                 entriesInHash, waveHashEntries,
			                 (entriesInHash*100.0)/waveHashEntries);
			}
#if (poolCacheBytes != 0)
		if (dbgPoolStats)
			fprintf (stderr, "%u entries in pool, using %s bytes\n",
			                 entriesInPool,
			                 int_to_string(poolCacheBytes-bytesLeftInPool));
#endif // poolCacheBytes

		if ((dbgRecurrenceStats) && (edgesExpanded != 0))
			fprintf (stderr, "%.1f new edges per edge (%jd/%d)\n",
			                 numExpansions / ((float) edgesExpanded),
			                 numExpansions, edgesExpanded);
		}

	// figure out how big the wave is

	waveSize    = current_to_previous ();
	ways        = count_closed_wave ();

	printf ("%dx%d --> " ucountFmt " ways (%s)\n",
	        N, R, fmt_ucount(ways), elapsed_time());


	if (deallocOnExit)
		{
		free (waveHash);
		free (wavePrevious);
		free (waveCurrent);
#if (poolCacheBytes != 0)
		free (waveExpansions);
		free (expansionsPool);
#endif // poolCacheBytes
		}

	return 0;
	}

//----------
//
// initialize_wave--
//	Generate all possible initial edge states for the top row of the grid.
//
// Since there is no history above the top row, we can create these states by
// considering all 2^(N-1) possibilites for the vertical walls in the row.
//
//----------
//
// Arguments:
//	(none)
//
// Returns:
//	(nothing)
//
// Side effects:
//	waveHash is cleared to zeros, then several (state,current_count) entries
//	are created
//
//----------

static void initialize_wave
   (void)
	{
	int			numWalls;
	u32			bits, wallBits, revWallBits, maxWallBits;
	unpackedge	u;
	packedge	e;
	u8			rgn, w;
	ucount		ways;
	int			regionTooBig;
	int			col;

	initialize_wave_hash ();

	numWalls = N-1;
	maxWallBits = (1 << numWalls) - 1;
	for (wallBits=0 ; wallBits<=maxWallBits ; wallBits++)
		{
		revWallBits = reverse_bits (numWalls, wallBits);
		if (revWallBits > wallBits) continue;
		ways = (revWallBits == wallBits)? to_ucount(1) : to_ucount(2);

		u.col = 0;
		u.regions[0] = rgn = 0;
		u.sizeOf[rgn] = 1;
		clear_adjacencies (&u);

		regionTooBig = false;
		for (col=1,bits=wallBits ; col<N ; col++,bits>>=1)
			{
			if ((bits & 1) == 0)
				{
				u.regions[col] = rgn;
				if (P == 0)
					{ if (u.sizeOf[rgn] < Q) u.sizeOf[rgn]++; }
				else
					{
					u.sizeOf[rgn]++;
					if (u.sizeOf[rgn] > P)
						{ regionTooBig = true;  break; }
					}
				}
			else
				{
				w = rgn;
				u.regions[col] = ++rgn;
				u.adjacent[rgn][w  ] = true;
				u.adjacent[w  ][rgn] = true;
				u.sizeOf[rgn] = 1;
				}
			}

		if (regionTooBig) continue;

		if (dbgInitialize)
			fprintf (stderr, ucountFmt " ways for " edgeFmt "\n",
			                 fmt_ucount(ways), unpackedge_to_string(&u));

		pack_edge (&u, &e);
		add_to_wave_hash (&e, ways);
		}
	}

//----------
//
// iterated_expansion--
//	Compute the successor states, a.k.a. expansions, of an edge state, stepping
//	through several columns.
//
//----------
//
// Arguments:
//	packedge*	e:			(pointer to) The edge state to expand.
//	int			numSteps:	Number of columns to step through.
//
// Returns:
//	A list of the expanded edge states.  (see note 1)
//
//----------
//
// notes:
//
// (1)	The memory containing the returned list of edges belongs to this
//		routine, as static memory.  There is only one such memory block (well
//		two, but only one that counts), and it is used on all subsequent calls.
//		When you make more than one call, the results of previous calls are
//		clobbered.
//
//----------

static edgelist* iterated_expansion
   (packedge*	e,
	int			numSteps)
	{
	static edgelistrow elRow;
	static edgelistrow elRow2;
	edgelist*	wavelet    = (edgelist*) &elRow;
	edgelist*	newWavelet = (edgelist*) &elRow2;
	edgelist*	newEdges;
	u32			newWaveletLen;
	int			step;
	u32			wIx;

	if (dbgExpansionIter)
		{
		fprintf (stderr, "==========\n");
		fprintf (stderr, "expanding " edgeFmt "\n", packedge_to_string(e));
		}

	// put the single source state in the current wavelet

	wavelet->len = 1;
	wavelet->e[0] = *e;

	// iterative expand the wave, column-by-column

	for (step=0 ; step<numSteps ; step++)
		{
		if (wavelet->len == 0) break;

		newWaveletLen = 0;
		for (wIx=0 ; wIx<wavelet->len ; wIx++)
			{
			if (dbgExpansionIter)
				fprintf (stderr, "  == step %d, wavelet[%d] = <" edgeFmt ">\n",
				                  step+1, wIx, packedge_to_string(&wavelet->e[wIx]));

			newEdges = expand_edge (&wavelet->e[wIx]);

			if (newWaveletLen + newEdges->len >= maxExpansionsInRow)
				{ fprintf (stderr, "wavelet exceeded limit\n");  exit (1); }
			memcpy (&newWavelet->e[newWaveletLen], newEdges->e,
			        newEdges->len * sizeof(packedge));
			newWaveletLen += newEdges->len;
		// the memcpy code above replaces this loop
		//	for (eIx=0 ; eIx<newEdges->len ; eIx++)
		//		{
		//		if (newWaveletLen >= maxExpansionsInRow)
		//			{ fprintf (stderr, "wavelet exceeded limit\n");  exit (1); }
		//		newWavelet->e[newWaveletLen++] = newEdges->e[eIx];
		//		}
			}

		newWavelet->len = newWaveletLen;
		if (wavelet == (edgelist*) &elRow)
			{ wavelet = (edgelist*) &elRow2; newWavelet = (edgelist*) &elRow; }
		else
			{ wavelet = (edgelist*) &elRow;  newWavelet = (edgelist*) &elRow2; }
		}

	if (dbgExpansionIter)
		fprintf (stderr, "  == wavelet has %d edges\n", wavelet->len);

	if (dbgShowRecurrence)
		{
		fprintf (stderr, "==========\n");
		fprintf (stderr, edgeFmt " -->\n", packedge_to_string(e));
		for (wIx=0 ; wIx<wavelet->len ; wIx++)
			fprintf (stderr, "  --> " edgeFmt "\n",
			                  packedge_to_string(&wavelet->e[wIx]));
		}

	return wavelet;
	}

//----------
//
// expand_edge--
//	Compute the successor states, a.k.a. expansions, of an edge state.
//
//----------
//
// Arguments:
//	packedge*	e:	(pointer to) The edge state to expand.
//
// Returns:
//	A list of the expanded edge states.  (see note 1)
//
//----------
//
// $$$ provide more detail here, with diagrams
//
// The process is broken into two stages.  In the first stage we determine
// which of the four wall configurations are legal;  in the second stage we
// create a new edge state for each of those legal wall configurations.
//
// The wall configurations are stored as a list of bits, according to this
// table:
//
//		+--------+--------+     +------------+------------+--------------+
//		| wall   | bit    |     | west       | north      | west*north^2 |
//		+--------+--------+     +------------+------------+--------------+
//		| closed | 01 = 1 |     | closed = 1 | closed = 1 |   1 = 0001   |
//		| open   | 10 = 2 |     | open   = 2 | closed = 1 |   2 = 0010   |
//		+--------+--------+     | closed = 1 | open   = 2 |   4 = 0100   |
//		                        | open   = 2 | open   = 2 |   8 = 1000   |
//		                        +------------+------------+--------------+
//
//----------
//
// notes:
//
// (1)	The memory containing the returned list of edges belongs to this
//		routine, as static memory.  There is only one such memory block, and
//		it is used on all subsequent calls.  When you make more than one call,
//		the results of previous calls are clobbered.
//
//----------

// sanity checks (if dbgSanityExpansion is NOT defined, these evaporate)

#ifndef dbgSanityExpansion
#define expansion_sanity_check(id,test) ;
#endif // not dbgSanityExpansion

#ifdef dbgSanityExpansion

#define expansion_sanity_check(id,test)                                       \
	if (!(test))                                                              \
		{                                                                     \
		fprintf (stderr, "expand_edge failed test \"" id "\"\n"               \
		                 "%08X %08X %08X\n"                                   \
		                 "<" edgeFmt ">\n",                                   \
		                 e->v[2], e->v[1], e->v[0],                           \
		                 unpackedge_to_string(&u));                           \
		exit (1);                                                             \
		}

#endif // dbgSanityExpansion


// other debug-output macros

#define various_debug_reports(wallPair)                                       \
	if (dbgExpansion)                                                         \
		fprintf (stderr, "[" wallPair "] --> " edgeFmt "\n",                  \
		                 unpackedge_to_string(&uNew));                        \
	                                                                          \
	if (dbgExpansionDraw)                                                     \
		{                                                                     \
		fprintf (stderr, "\n");                                               \
		draw_unpackedge (stderr, &uNew, "    ");                              \
		fprintf (stderr, "\n");                                               \
		}                                                                     \
	                                                                          \
	if (dbgExpansionLog)                                                      \
		fprintf (stderr, "\t" edgeLogFmt " [" wallPair "] --> " edgeFmt "\n", \
		                 unpackedge_to_string(&u),                            \
		                 unpackedge_to_string(&uNew));                        \
	                                                                          \
	if ((dbgUnpack) || (dbgUnpack))                                           \
		{                                                                     \
		if (strcmp(unpackedge_to_string(&uNew),                               \
				   packedge_to_string(&edges->e[numExpansions-1])) != 0)      \
			{                                                                 \
			fprintf (stderr, "FAILED PACK/UNPACK\n");                         \
			exit (1);                                                         \
			}                                                                 \
		}


// wall definitions (see description above)

#define wall_closed 1
#define wall_open   2

#define walls_WcNc  1	// W closed  N closed
#define walls_WoNc  2	// W open    N closed
#define walls_WcNo  4	// W closed  N open
#define walls_WoNo  8	// W open    N open


// routine to test whether a region is "trapped"

static int trapped (u8* regions, u8 nonCol, u8 rgn);
static int trapped (u8* regions, u8 nonCol, u8 rgn)
	{
	u8 ix;
	for (ix=0 ; ix<N ; ix++)
		{ if ((ix!=nonCol) && (regions[ix]==rgn)) return false; }
	return true;
	}


// expand_edge--

static edgelist* expand_edge
   (packedge* e)
	{
	static edgelist4 el4;
	edgelist*	edges = (edgelist*) &el4;
	unpackedge	u, uNew;
	u8			wallPairs;			// bit list of walls_XXX values
	u8			wallPair;
	u8			wWalls, nWalls;		// bit list of wall_XXX values
	u8			wWall, wWallMin, wWallMax;
	u8			nWall, nWallMin, nWallMax;
	u8			rgn, maxRgn;
	u8			sIx, wIx, s, w;
	u8			sSize, wSize;
	u8			col, nextCol;
	int			cantMerge, nActive;
	u32			numExpansions;

	unpack_edge (e, &u);

	if (dbgExpansion)
		{
		fprintf (stderr, "==========\n");
		fprintf (stderr, "expanding " edgeFmt "\n", unpackedge_to_string(&u));
		}

	if (dbgExpansionDraw)
		{
		fprintf (stderr, "\n");
		draw_unpackedge (stderr, &u, "    ");
		fprintf (stderr, "\n");
		}

	// determine the possible wall configurations for the new cell;  note that
	// we call the south and west neighbors "s" and "w"

	if (u.col == 0)
		{ sIx = u.col;                  s = u.regions[sIx];  w = noRegion;       }
	else
		{ sIx = u.col;  wIx = u.col-1;  s = u.regions[sIx];  w = u.regions[wIx]; }

	sSize = u.sizeOf[s];

	if (u.col == 0)									// we're at the west end
		{
		if (dbgWalls)
			fprintf (stderr, "(west end)\n");
		if (is_full(sSize))
			{
			wallPairs = walls_WcNc;					// (s can't grow)
			if (dbgWalls)
				fprintf (stderr, "(N=%c is full)\n", s+'A');
			}
		else if ((sSize < Q) && (trapped(u.regions,sIx,s)))
			{
			wallPairs = walls_WcNo;					// (s must grow)
			if (dbgWalls)
				fprintf (stderr, "(N=%c is trapped)\n", s+'A');
			}
		else
			wallPairs = walls_WcNc + walls_WcNo;	// (s may grow)
		}
	else if (s == w)								// (new cell must connect
		{											//  ..  both or neither)
		if (dbgWalls)
			fprintf (stderr, "(N and W same)\n");
		if (is_full(sSize))
			{
			wallPairs = walls_WcNc;					// (can't grow)
			if (dbgWalls)
				fprintf (stderr, "(N/W=%c is full)\n", s+'A');
			}
		else
			wallPairs = walls_WcNc + walls_WoNo;	// (can grow)
		}
	else											// general case
		{
		if (dbgWalls)
			fprintf (stderr, "(general case)\n");

		wSize = u.sizeOf[w];

		if (is_full(wSize))
			{
			wWalls = wall_closed;
			if (dbgWalls)
				fprintf (stderr, "(W=%c is full)\n", w+'A');
			}
		else
			wWalls = wall_closed + wall_open;

		if (is_full(sSize))
			{
			nWalls = wall_closed;
			if (dbgWalls)
				fprintf (stderr, "(N=%c is full)\n", s+'A');
			}
		else if ((sSize < Q) && (trapped(u.regions,sIx,s)))
			{
			nWalls = wall_open;
			if (dbgWalls)
				fprintf (stderr, "(N=%c is trapped)\n", s+'A');
			}
		else
			nWalls = wall_closed + wall_open;

		cantMerge = ((u.adjacent[w][s]) || ((P != 0) && (sSize+wSize >= P)));

		if      (wWalls == wall_closed) wWallMin = wWallMax = wall_closed;
		else if (wWalls == wall_open)   wWallMin = wWallMax = wall_open;
		else                  { wWallMin = wall_closed;  wWallMax = wall_open; }

		if      (nWalls == wall_closed) nWallMin = nWallMax = wall_closed;
		else if (nWalls == wall_open)   nWallMin = nWallMax = wall_open;
		else                  { nWallMin = wall_closed;  nWallMax = wall_open; }

		wallPairs = 0;
		for (wWall=wWallMin; wWall<=wWallMax ; wWall++)
				for (nWall=nWallMin; nWall<=nWallMax ; nWall++)
			{
			wallPair = wWall * nWall * nWall;
			if ((cantMerge) && (wallPair == walls_WoNo)) continue;
			wallPairs += wallPair;
			}
		if (dbgWalls)
			fprintf (stderr, "(wWalls=%d  nWalls=%d  wallPairs=%d)\n",
			                 wWalls, nWalls, wallPairs);
		}

	// for each possible wall configuration, determine the new state

	numExpansions = 0;
	nextCol = (u.col+1) % N;
	maxRgn  = number_of_regions (&u) - 1;

	if ((wallPairs & walls_WcNc) != 0)				//  W closed  N closed
		{
		expansion_sanity_check ("W closed N closed",
		                        ((sSize >= Q) || (!trapped(u.regions,sIx,s))));

		copy_unpackedge (&u, &uNew);
		uNew.col = nextCol;

		nActive = (!trapped(u.regions,sIx,s));
		if (!nActive)								// remove region s
			{
			uNew.sizeOf[s] = 0;
			for (rgn=0 ; rgn<=maxRgn ; rgn++)
				uNew.adjacent[rgn][s] = uNew.adjacent[s][rgn] = false;
			}

		uNew.regions[u.col] = rgn = maxRgn + 1;
		uNew.sizeOf[rgn] = 1;
		if (w != noRegion) uNew.adjacent[rgn][w] = uNew.adjacent[w][rgn] = true;
		if (nActive)       uNew.adjacent[rgn][s] = uNew.adjacent[s][rgn] = true;

		normalize_edge (&uNew);
		pack_edge (&uNew, &edges->e[numExpansions++]);

		various_debug_reports ("|-");
		}

	if ((wallPairs & walls_WoNc) != 0)				//  W open    N closed
		{
#if (P != 0)
		expansion_sanity_check ("W open N closed, A", (u.sizeOf[w] < P));
#endif
		expansion_sanity_check ("W open N closed, B",
		                        ((sSize >= Q) || (!trapped(u.regions,sIx,s))));

		copy_unpackedge (&u, &uNew);
		uNew.col = nextCol;

		nActive = (!trapped(u.regions,sIx,s));
		if (!nActive)								// remove region s
			{
			uNew.sizeOf[s] = 0;
			for (rgn=0 ; rgn<=maxRgn ; rgn++)
				uNew.adjacent[rgn][s] = uNew.adjacent[s][rgn] = false;
			}

		uNew.regions[u.col] = w;
		if ((P != 0) || (uNew.sizeOf[w] < Q)) uNew.sizeOf[w]++;
		if (nActive) uNew.adjacent[w][s] = uNew.adjacent[s][w] = true;

		normalize_edge (&uNew);
		pack_edge (&uNew, &edges->e[numExpansions++]);

		various_debug_reports (".-");
		}

	if ((wallPairs & walls_WcNo) != 0)				//  W closed  N open
		{
#if (P != 0)
		expansion_sanity_check ("W closed N open", (sSize < P));
#endif

		copy_unpackedge (&u, &uNew);
		uNew.col = nextCol;

		if ((P != 0) || (uNew.sizeOf[s] < Q)) uNew.sizeOf[s]++;
		if (w != noRegion) uNew.adjacent[w][s] = uNew.adjacent[s][w] = true;

		normalize_edge (&uNew);
		pack_edge (&uNew, &edges->e[numExpansions++]);

		various_debug_reports ("|.");
		}

	if ((wallPairs & walls_WoNo) != 0)				//  W open    N open
		{
#if (P != 0)
		expansion_sanity_check ("W open N open, B", ((sSize < P) && (!u.adjacent[w][s])));
		expansion_sanity_check ("W open N open, B", ((s == w) || (sSize+u.sizeOf[w] < P)));
#endif

		copy_unpackedge (&u, &uNew);
		uNew.col = nextCol;

		if (s == w)
			{ if ((P != 0) || (uNew.sizeOf[s] < Q)) uNew.sizeOf[s]++; }
		else										// merge region w and s;
			{										// .. change all s's to w's
			if ((P != 0) || (uNew.sizeOf[w] + sSize + 1 < Q))
				uNew.sizeOf[w] += sSize + 1;
			else
				uNew.sizeOf[w] = Q;
			uNew.sizeOf[s] = 0;
			for (col=0 ; col<N ; col++)
				{ if (uNew.regions[col] == s) uNew.regions[col] = w; }

			for (rgn=0 ; rgn<=maxRgn ; rgn++)
				{
				if (uNew.adjacent[s][rgn])
					uNew.adjacent[rgn][w] = uNew.adjacent[w][rgn] = true;
				uNew.adjacent[rgn][s] = uNew.adjacent[s][rgn] = false;
				}
			}

		normalize_edge (&uNew);
		pack_edge (&uNew, &edges->e[numExpansions++]);
		various_debug_reports ("..");
		}

	edges->len = numExpansions;
	return edges;
	}

//----------
//
// edge_flippage--
//	Determine the symmetry properties of an edge state.
//
//----------
//
// Arguments:
//	unpackedge*	u:	(pointer to) The edge state to test.  We assume (but do not
//					.. verify) that regions are in in left-right order.
//
// Returns:
//	positive: state is non-symmetric, preferred to its flip-state
//	zero:     state is symmetric
//	negative: state is non-symmetric, its flip-state is preferred
//
// Byproducts:
//	flipRegionMap is filled, suitable for mapping regions to the flipped state.
//
//----------

// comparison function for qsort, comparing 2-letter strings

int qComparePairs (const void* _pairA, const void* _pairB);
int qComparePairs (const void* _pairA, const void* _pairB)
	{
	char* pairA = (char*) _pairA;
	char* pairB = (char*) _pairB;
	int   diff;

	diff = ((int) pairA[0]) - ((int) pairB[0]);
	if (diff != 0) return diff;

	return ((int) pairA[1]) - ((int) pairB[1]);
	}


// table to map regions to the flipped counterpart (filled as a byproduct)

static u8 flipRegionMap[N];


// edge_flippage--

static int edge_flippage
   (unpackedge*	u)
	{
	char		pairs[(2*Nc2)+1], revPairs[(2*Nc2)+1];
	int			col;
	u8			oldRgn, newRgn, maxRgn;
	u8			rgn, revRgn, rgn1, rgn2;
	int			numPairs, diff;
	u8			size, revSize;

	// only full rows can be symmetric

	if (u->col != 0) return 1;

	// if symmetry folding is disabled, just call every state symmetric

	if (dbgNoFlippage) return 1;

	// determine region assignments, so that the regions would be right-left
	// ordered

	for (oldRgn=0 ; oldRgn<N ; oldRgn++)
		flipRegionMap[oldRgn] = noRegion;

	newRgn = 0;
	for (col=N-1 ; col>=0 ; col--)
		{
		oldRgn = u->regions[col];
		if (flipRegionMap[oldRgn] != noRegion) continue;
		flipRegionMap[oldRgn] = newRgn;
		newRgn++;
		}

	maxRgn = newRgn - 1;

	// compare regions to flipped regions;  we prefer the lexicographically
	// lower string

	for (col=0 ; col<N ; col++)
		{
		rgn    = u->regions[col];
		revRgn = flipRegionMap[u->regions[(N-1)-col]];
		if (rgn < revRgn) return  1;
		if (rgn > revRgn) return -1;
		}

	// compare sizes to those of flipped regions;  we prefer larger regions
	// on the left

	for (rgn=0 ; rgn<=maxRgn ; rgn++)
		{
		size    = u->sizeOf[rgn];
		revSize = u->sizeOf[flipRegionMap[rgn]];
		if (size > revSize) return  1;
		if (size < revSize) return -1;
		}

	// compare adjacency list to those of flipped regions;  we prefer
	// adjacencies toward the left

	numPairs = 0;

	for (rgn1=0 ; rgn1<maxRgn ; rgn1++)
			for (rgn2=rgn1+1 ; rgn2<=maxRgn ; rgn2++)
		{
		if (!u->adjacent[rgn1][rgn2]) continue;

		pairs[2*numPairs  ] = 'A' + rgn1;
		pairs[2*numPairs+1] = 'A' + rgn2;

		if (flipRegionMap[rgn1] < flipRegionMap[rgn2])
			{
			revPairs[2*numPairs  ] = 'A' + flipRegionMap[rgn1];
			revPairs[2*numPairs+1] = 'A' + flipRegionMap[rgn2];
			}
		else
			{
			revPairs[2*numPairs  ] = 'A' + flipRegionMap[rgn2];
			revPairs[2*numPairs+1] = 'A' + flipRegionMap[rgn1];
			}

		numPairs++;
		}

	pairs   [2*numPairs] = 0;
	revPairs[2*numPairs] = 0;

	qsort (pairs,    numPairs, 2, qComparePairs);
	qsort (revPairs, numPairs, 2, qComparePairs);

	diff = strcmp (pairs,revPairs);
	if (diff != 0) return -diff;

 	// the state must be symmetric
 
	return 0;
	}

//----------
//
// normalize_edge--
//	Reduce a generalized edge state to its "normal" form.
//
//----------
//
// Arguments:
//	unpackedge*	u:	(pointer to) The edge state to normalize.
//
// Returns:
//	(nothing)
//
//----------
//
// A normalized state has its regions in left-right order.  For example,
// AABACCDDE is in left-right order, because the first occurence of any region
// has only (alphabetically) lower regions to its left.  AABADDEEC is an
// example that is NOT in left-right order.
//
// Further, a normalized state has no missing regions.  An example of missing
// regions is AACADDEEF, because B is missing.
//
// Adjacencies are also modified so that any region that is full (has size P)
// has no adjacencies.  In fact the region is certainly adjacent to others.
// But we have no reason to include this information in the state, because the
// adjacency information is only used to prevent us from merging regions that
// are already adjacent.  Since we can't merge a full region with any other, we
// can remove this information.  This allows us to combine some states that we
// wouldn't otherwise be able to.
//
// We also fold symmetrical equivalents.  If the state's "flippage" is
// negative, we flip the state left-for-right.
//
// $$$ this can be improved by combining all regions that are full into a
// $$$ .. single region
//
//----------

static void normalize_edge
   (unpackedge*	u)
	{
	unpackedge	uu;
	u8			rgnMap[N+1];
	u8			oldRgn, newRgn, maxRgn, maxOldRgn;
	u8			oldRgn1, oldRgn2, newRgn1, newRgn2;
	u8			col;

	// determine region assignments, so that the regions will be left-right
	// ordered;  note that we allow N+1 entries because the incoming edge
	// might have an extra region

	if (separateFulls)
		{
		for (oldRgn=0 ; oldRgn<=N ; oldRgn++)
			rgnMap[oldRgn] = noRegion;

		newRgn = 0;  maxOldRgn = u->regions[0];
		for (col=0 ; col<N ; col++)
			{
			oldRgn = u->regions[col];
			if (rgnMap[oldRgn] != noRegion) continue;
			rgnMap[oldRgn] = newRgn;
			if (oldRgn > maxOldRgn) maxOldRgn = oldRgn;
			newRgn++;
			}

		maxRgn = newRgn - 1;
		}
	else
		{
		int fullRgn = -1;
		
		for (oldRgn=0 ; oldRgn<=N ; oldRgn++)
			rgnMap[oldRgn] = noRegion;

		newRgn = 0;  maxOldRgn = u->regions[0];
		for (col=0 ; col<N ; col++)
			{
			oldRgn = u->regions[col];
			if (rgnMap[oldRgn] != noRegion) continue;

			if (is_full(u->sizeOf[oldRgn]))
				{
				if (fullRgn >= 0)
					{
					rgnMap[oldRgn] = rgnMap[fullRgn];
					if (oldRgn > maxOldRgn) maxOldRgn = oldRgn;
					continue;
					}
				fullRgn = oldRgn;
				}

			rgnMap[oldRgn] = newRgn;
			if (oldRgn > maxOldRgn) maxOldRgn = oldRgn;
			newRgn++;
			}

		maxRgn = newRgn - 1;
		}

	// map the original into a local copy

	uu.col = u->col;

	for (col=0 ; col<N ; col++)
		uu.regions[col] = rgnMap[u->regions[col]];

	for (oldRgn=0 ; oldRgn<=maxOldRgn ; oldRgn++)
		{
		newRgn = rgnMap[oldRgn];
		if (newRgn == noRegion) continue;
		uu.sizeOf[newRgn] = u->sizeOf[oldRgn];
		}

	clear_adjacencies (&uu);
	for (oldRgn1=0 ; oldRgn1<maxOldRgn ; oldRgn1++)
		{
		newRgn1 = rgnMap[oldRgn1];
		if (newRgn1 == noRegion) continue;
		if (is_full(uu.sizeOf[newRgn1])) continue;
		for (oldRgn2=oldRgn1+1 ; oldRgn2<=maxOldRgn ; oldRgn2++)
			{
			newRgn2 = rgnMap[oldRgn2];
			if (newRgn2 == noRegion) continue;
			if (is_full(uu.sizeOf[newRgn2])) continue;
			if (!u->adjacent[oldRgn1][oldRgn2]) continue;
			uu.adjacent[newRgn1][newRgn2] = true;
			uu.adjacent[newRgn2][newRgn1] = true;
			}
		}

	// fold symmetric equivalents

	if ((uu.col != 0) || (edge_flippage (&uu) >= 0))
		copy_unpackedge (&uu, u);	// just move the local copy into original
	else
		{ // map the local copy into the original, using flipped region map

		for (col=0 ; col<N ; col++)
			u->regions[col] = flipRegionMap[uu.regions[(N-1)-col]];

		for (oldRgn=0 ; oldRgn<=maxRgn ; oldRgn++)
			{
			newRgn = flipRegionMap[oldRgn];
			u->sizeOf[newRgn] = uu.sizeOf[oldRgn];
			}

		clear_adjacencies (u);
		for (oldRgn1=0 ; oldRgn1<maxRgn ; oldRgn1++)
			{
			if (is_full(uu.sizeOf[oldRgn1])) continue;
			newRgn1 = flipRegionMap[oldRgn1];
			for (oldRgn2=oldRgn1+1 ; oldRgn2<=maxRgn ; oldRgn2++)
				{
				if (is_full(uu.sizeOf[oldRgn2])) continue;
				newRgn2 = flipRegionMap[oldRgn2];
				if (!uu.adjacent[oldRgn1][oldRgn2]) continue;
				u->adjacent[newRgn1][newRgn2] = true;
				u->adjacent[newRgn2][newRgn1] = true;
				}
			}
		}

	}

//----------
//
// count_closed_wave--
//	Count the ways that edges in the current wave can be 'closed', by closing
//	off the upper row.
//
//----------
//
// Arguments:
//	(none)
//
// Returns:
//	The total of previous counts in the hash, for edges that are closed.
//
//----------

static ucount count_closed_wave
   (void)
	{
	packedge*	e;
	unpackedge	u;
	ucount		ways, totalWays;
	u32			h;

	totalWays = to_ucount(0);
	h = preHashIndex;
	while ((e = next_in_previous_wave (&h, &ways, false)) != NULL)
		{
		unpack_edge (e, &u);
		if (!edge_is_closed (&u)) continue;

//		if (dbgBigArithmetic)
//			{
//			ucount sum = totalWays;
//			accumulate(sum,ways);
//			if ((totalWays.ms != 0) || (ways.ms != 0) || (sum.ms != 0))
//				{
//				printf ("%08X %016jX + %08X %016jX = %08X %016jX   %31s + %31s = %31s\n",
//				        totalWays.ms, totalWays.ls, ways.ms, ways.ls, sum.ms, sum.ls,
//				        ucount_to_string(&totalWays), ucount_to_string(&ways), ucount_to_string(&sum));
//				}
//			}

		accumulate(totalWays,ways);
		}

	return totalWays;
	}

//----------
//
// edge_is_closed--
//	Determine if all regions in an edge state can be 'closed', by closing off
//	the upper row.
//
//----------
//
// Arguments:
//	unpackedge*	u:	(pointer to) The edge state to inspect.
//
// Returns:
//	(nothing)
//
//----------

static int edge_is_closed
   (unpackedge*	u)
	{
	u8			rgn, maxRgn;

	if (u->col != 0) return false;

	maxRgn = number_of_regions(u);
	for (rgn=0 ; rgn<maxRgn ; rgn++)
		{ if (u->sizeOf[rgn] < Q) return false; }

	return true;
	}

//----------
//
// pack_edge--
//	Compute the packed value of an unpacked edge state.
//
//----------
//
// Arguments:
//	unpackedge*	u:	(pointer to) The edge to pack.
//	packedge*	e:	(pointer to) Place to store the packed edge.
//
// Returns:
//	(nothing)
//
//----------
//
// $$$ update the following to reflect
// $$$ .. trackAdjacency == false => we encode less about adajacency
//
// The packing is performed in fields, with the first fields encoded in the
// least significant bits of the 96-bit value (e->v[0] has the 32 least
// significant bits).  We show fields left to right in this description, with
// the left fields being the least significant.
//
// The full packed value is
//		<col> <regions> <sizes> <adjacencies>
//
// <col> is a 4-bit value, 0 to N-1
//
// <regions> is encoded as N-1 3- or 4-bit tokens.  We assume the first region
// is a zero, then encode the next region as
//		000  => same as previous region
//		010  => one more than max region so far
//		100  => one less than max region so far
//		110  => two less than max region so far
//		xxx1 => 3+xxx less than max region so far (3..10 less)
// Note that <regions> cannot encode to all zero bits.  Maximum size for
// <regions> for N=9 should be 26 bits, as we can't have more than two of the
// 4-bit tokens.
//
// <sizes> is encoded as K 4-bit values, where K is the number of regions
// present.  Since all values are positive, the encoding subtracts one from
// each size.  Maximum size for <sizes> for N=9 is 36 bits.
//
// <adjacencies> is encoded as 1 bit for each of (L-1)-choose-2 region pairs,
// where L is the number of non-full regions present (regions with size less
// than P).  We do not encode pairs for consecutive regions (e.g. region 2 vs
// 3);  instead, we presume such regions are adjacent IF they occupy
// consecutive cells in the region list.  One extra bit tells us whether the
// consecutive regions at the jagged column are adjacent or not (this bit is
// only encoded if col!=0 and regions[col-1]+1==regions[col], and neither
// region is full).  For all bits, 1  means adjacent, 0 means not.  Maximum
// size for <adjacencies> for N=9 is 29 bits.
//
// For N=9, the maximum total size is 4+26+36+29 = 95 bits.
//
//----------

#if (N >= 10)
#error ***** pack_edge assumes numColumns < 10 *****
#endif

#if ((P != 0) && (P > 16))
#error ***** pack_edge assumes maxPiece <= 16 *****
#endif

#if ((P == 0) && (Q > 16))
#error ***** pack_edge assumes minPiece <= 16 *****
#endif


#define rgnsize_same     0 // 000
#define rgnsize_plusOne  2 // 010
#define rgnsize_minusOne 4 // 100
#define rgnsize_minusTwo 6 // 110


#define left_shift_bits(n)                 \
    more = (more<<(n)) + (bits>>(64-(n))); \
    bits = (bits<<(n));


static void pack_edge
   (unpackedge*	u,
	packedge*	e)
	{
	u64			bits = 0;
	u32			more = 0;
	int			rgnTokens[N], rgnShifts[N];
	int			rgn, rgn1, rgn2, maxRgn;
	int			col;
	u8			xxx;

	if (dbgPack)
		fprintf (stderr, "packing " edgeFmt "\n", unpackedge_to_string(u));

	// encode <adjacencies>
	// $$$ need to keep a count of bits used and check for overflow

	maxRgn = u->regions[0];
	for (col=1 ; col<N ; col++)
		{ if (u->regions[col] > maxRgn) maxRgn = u->regions[col]; }

	if (trackAdjacency)
		{
		for (rgn1=maxRgn-2 ; rgn1>=0 ; rgn1--)
			{
			if (is_full(u->sizeOf[rgn1])) continue;
			for (rgn2=maxRgn ; rgn2>=rgn1+2 ; rgn2--)
				{
				if (is_full(u->sizeOf[rgn2])) continue;
				left_shift_bits(1);
				if (u->adjacent[rgn1][rgn2]) bits++;
				if (dbgPack)
					fprintf (stderr, "  adj[%c%c] = %d\n",
									 'A'+rgn1, 'A'+rgn2,
									 u->adjacent[rgn1][rgn2]);
				}
			}
		}
	else
		{ // $$$ this could be improved
		int rgnA = u->regions[u->col-1];
		int rgnB = u->regions[u->col];
		if ((rgnA == rgnB) || (rgnA+1 == rgnB) || (!u->adjacent[rgnA][rgnB]))
			rgnA = rgnB = -1;

		for (rgn1=maxRgn-2 ; rgn1>=0 ; rgn1--)
			{
			if (is_full(u->sizeOf[rgn1])) continue;
			for (rgn2=maxRgn ; rgn2>=rgn1+2 ; rgn2--)
				{
				if (is_full(u->sizeOf[rgn2])) continue;
				left_shift_bits(1);
				if      ((rgn1 == rgnA) && (rgn2 == rgnB)) bits++;
				else if ((rgn1 == rgnB) && (rgn2 == rgnA)) bits++;
				if (dbgPack)
					fprintf (stderr, "  adj[%c%c] = %d\n",
									 'A'+rgn1, 'A'+rgn2,
									 (int)(bits & 1));
				}
			}
		}

	if ((u->col != 0) && (u->regions[u->col-1]+1 == u->regions[u->col]))
		{ // encode jagged column adjacency
		rgn1 = u->regions[u->col-1];
		rgn2 = u->regions[u->col];
		if ((!is_full(u->sizeOf[rgn1])) && (!is_full(u->sizeOf[rgn2])))
			{
			left_shift_bits(1);
			if (u->adjacent[rgn1][rgn2]) bits++;
			if (dbgPack)
				fprintf (stderr, "  adj[%c%c] = %d (jagged)\n",
				                 'A'+rgn1, 'A'+rgn2,
				                 u->adjacent[rgn1][rgn2]);
			}
		}

	// encode <sizes>

	for (rgn=maxRgn ; rgn>=0 ; rgn--)
		{
		left_shift_bits(4);  bits += u->sizeOf[rgn]-1;
		if (dbgPack)
			fprintf (stderr, "  size[%c] = %s\n",
			                 'A'+rgn, bits_string(4,u->sizeOf[rgn]-1));
		}

	// encode <regions>

	rgn = maxRgn = 0;
	for (col=1 ; col<N ; col++)
		{
		rgnShifts[col] = 3;
		if      (u->regions[col] == rgn)        rgnTokens[col] = rgnsize_same;
		else if (u->regions[col] == maxRgn+1) { rgnTokens[col] = rgnsize_plusOne;  ++maxRgn; }
		else if (u->regions[col] == maxRgn-1)   rgnTokens[col] = rgnsize_minusOne;
		else if (u->regions[col] == maxRgn-2)   rgnTokens[col] = rgnsize_minusTwo;
		else
			{ // $$$ add an assert that 0<=xxx<=7
			xxx = (maxRgn-3) - u->regions[col];
			rgnTokens[col] = (xxx << 1) + 1;
			rgnShifts[col] = 4;
			}

		rgn = u->regions[col];
		}

	for (col=N-1 ; col>0 ; col--)
		{
		left_shift_bits(rgnShifts[col]);  bits += rgnTokens[col];
		if (dbgPack)
			fprintf (stderr, "  rgn[%d]  = %s\n",
			                 col, bits_string(rgnShifts[col],rgnTokens[col]));
		}

	// encode <col>

	left_shift_bits(4);  bits += u->col;

	e->v[0] = bits & 0xFFFFFFFF;
	e->v[1] = bits >> 32;
	e->v[2] = more;

	if (dbgPack)
		fprintf (stderr, "  --> %08X %08X %08X\n", e->v[2], e->v[1], e->v[0]);

	// add 1 to ensure it is non-zero

	if (++e->v[0] == 0)
		{ if (++e->v[1] == 0) e->v[2]++; }
	}

//----------
//
// unpack_edge--
//	Compute the packed value of an unpacked edge state.
//
//----------
//
// Arguments:
//	packedge*	e:	(pointer to) The edge to unpack.
//	unpackedge*	u:	(pointer to) Place to store the unpacked edge.
//
// Returns:
//	(nothing)
//
//----------

#define right_shift_bits(n)                \
	bits = (bits>>(n)) + (((u64)more)<<(64-(n))); \
    more = (more>>(n));


static void unpack_edge
   (packedge*	e,
	unpackedge*	u)
	{
	u64			bits = (((u64)e->v[1]) << 32) + e->v[0];
	u32			more = e->v[2];
	int			rgn, rgn1, rgn2, maxRgn;
	int			col;
	u8			token, xxx;

	// subtract 1

	if (bits-- == 0) more--;

	if (dbgUnpack)
		fprintf (stderr, "unpacking %08X %08X %08X\n",
		                 more, (u32) (bits>>32), (u32) bits);

	// decode <col>

	u->col = bits & 0xF;  right_shift_bits(4);

	if (dbgUnpack)
		fprintf (stderr, "  col     = %s\n", bits_string(4,u->col));

	// decode <regions>

	u->regions[0] = rgn = maxRgn = 0;
	for (col=1 ; col<N ; col++)
		{
		token = bits & 0x7;
		if ((token & 1) == 0) {                      right_shift_bits(3); }
		                 else { token = bits & 0xF;  right_shift_bits(4); }

		switch (token)
			{
			case rgnsize_same:
				u->regions[col] = rgn;
				break;
			case rgnsize_plusOne:
				u->regions[col] = rgn = ++maxRgn;
				break;
			case rgnsize_minusOne:
				u->regions[col] = rgn = maxRgn-1;
				break;
			case rgnsize_minusTwo:
				u->regions[col] = rgn = maxRgn-2;
				break;
			default: // xxx1
				xxx = token >> 1;
				u->regions[col] = rgn = maxRgn - (3+xxx);
				break;
			}

		if (dbgUnpack)
			{
			if ((token & 1) == 0)
				fprintf (stderr, "  rgn[%d]  = %s\n", col, bits_string(3,token));
			else
				fprintf (stderr, "  rgn[%d]  = %s\n", col, bits_string(4,token));
			}
		}

	// decode <sizes>

	for (rgn=0 ; rgn<=maxRgn ; rgn++)
		{
		u->sizeOf[rgn] = (bits & 0xF) + 1;  right_shift_bits(4);
		if (dbgUnpack)
			fprintf (stderr, "  size[%c] = %s\n", 'A'+rgn, bits_string(4,u->sizeOf[rgn]-1));
		}

	// decode <adjacencies>

	clear_adjacencies (u);

	for (col=1 ; col<N ; col++)
		{
		if (col == u->col) continue;                        // (at jagged edge)
		rgn1 = u->regions[col-1];
		if (is_full(u->sizeOf[rgn1])) continue;             // (region 1 full)
		rgn2 = u->regions[col];
		if (is_full(u->sizeOf[rgn2])) continue;             // (region 2 full)
		if ((rgn1+1 != rgn2) && (rgn1 != rgn2+1)) continue; // (not consecutive)
		u->adjacent[rgn1][rgn2] = true;
		u->adjacent[rgn2][rgn1] = true;
		}

	if ((u->col != 0) && (u->regions[u->col-1]+1 == u->regions[u->col]))
		{ // decode jagged column adjacency
		rgn1 = u->regions[u->col-1];
		rgn2 = u->regions[u->col];
		if ((!is_full(u->sizeOf[rgn1])) && (!is_full(u->sizeOf[rgn2])))
			{
			if ((bits & 1) != 0)
				{
				u->adjacent[rgn1][rgn2] = true;
				u->adjacent[rgn2][rgn1] = true;
				}
			right_shift_bits(1);

			if (dbgUnpack)
				fprintf (stderr, "  adj[%c%c] = %d (jagged)\n",
								 'A'+rgn1, 'A'+rgn2, u->adjacent[rgn1][rgn2]);
			}
		}

	for (rgn1=0 ; rgn1<=maxRgn-2 ; rgn1++)
		{
		if (is_full(u->sizeOf[rgn1])) continue;
		for (rgn2=rgn1+2 ; rgn2<=maxRgn ; rgn2++)
			{
			if (is_full(u->sizeOf[rgn2])) continue;
			if ((bits & 1) != 0)
				{
				u->adjacent[rgn1][rgn2] = true;
				u->adjacent[rgn2][rgn1] = true;
				}
			right_shift_bits(1);

			if (dbgUnpack)
				fprintf (stderr, "  adj[%c%c] = %d\n",
								 'A'+rgn1, 'A'+rgn2, u->adjacent[rgn1][rgn2]);
			}
		}

	if (!trackAdjacency)
		{
		for (col=1 ; col<N ; col++)
			{
			if (col == u->col) continue;
			rgn1 = u->regions[col-1];
			if (is_full(u->sizeOf[rgn1])) continue;         // (region 1 full)
			rgn2 = u->regions[col];
			if (is_full(u->sizeOf[rgn2])) continue;         // (region 2 full)
			if (rgn1 == rgn2) continue;
			u->adjacent[rgn1][rgn2] = true;
			u->adjacent[rgn2][rgn1] = true;
			}
		}

	if (dbgPack)
		fprintf (stderr, " --> " edgeFmt "\n", unpackedge_to_string(u));
	}

//----------
//
// copy_unpackedge--
//	Copy an edge state.
//
//----------
//
// Arguments:
//	unpackedge*	src:	(pointer to) The edge to copy.
//	unpackedge*	dst:	(pointer to) Place to store the copy of the edge.
//
// Returns:
//	(nothing)
//
//----------

static void copy_unpackedge
   (unpackedge*	src,
	unpackedge* dst)
	{
	memcpy (dst, src, sizeof(unpackedge));
	}

//----------
//
// clear_adjacencies--
//	Erase an edge's adjacencies table.
//
//----------
//
// Arguments:
//	unpackedge*	u:	(pointer to) The edge state to operate upon.
//
// Returns:
//	(nothing)
//
//----------

static void clear_adjacencies
   (unpackedge*	u)
	{
	memset (u->adjacent, 0, sizeof(u->adjacent));
	}

//----------
//
// number_of_regions--
//	Determine the number of regions occupied by an edge.
//
// Note that we assume there are no unused symbols, so the number of regions
// occupied is simply the maximum region symbol, plus one.
//
//----------
//
// Arguments:
//	unpackedge*	u:	(pointer to) The edge state to operate upon.
//
// Returns:
//	The number of regions.
//
//----------

static u8 number_of_regions
   (unpackedge*	u)
	{
	u8			col, rgn, maxRgn;

	maxRgn = u->regions[0];
	for (col=1 ; col<N ; col++)
		{
		rgn = u->regions[col];
		if (rgn > maxRgn) maxRgn = rgn;
		}

	return maxRgn + 1;
	}

//----------
//
// string_to_packedge, string_to_unpackedge--
//	Convert a string to an edge state to a string.
//
//----------
//
// Arguments:
//	char*		s:	The string to convert (see detail below).
//	packedge*	e:	(pointer to) Place to store the edge.
//	 -OR-
//	unpackedge*	u:	(pointer to) Place to store the edge.
//
// Returns:
//	(nothing)
//
//----------
//
// The string looks like this:
//		ABAACDE  A7,B1,C3,D3,E1  AB,AC,CD,DE
// The third field can be missing if it would be empty.
//
//----------

#define good_char(ch) (((ch)!=0)&&(!isspace((ch))))
#define bad_char(ch)  (((ch)==0)||(isspace((ch))))


static void string_to_packedge
   (char*		s,
	packedge*	e)
	{
	unpackedge	u;

	string_to_unpackedge (s, &u);

	pack_edge (&u, e);
	}


static void string_to_unpackedge
   (char*		s,
	unpackedge*	u)
	{
	char*		regions, *adjacencies;
	char*		scan;
	int			slashCol, col;
	u8			rgn, rgn1, rgn2, maxRgn;
	int			needComma;
	char*		sizes;
	u8			size;

	// locate the space/separators

	scan = s;

	while ((*scan!=0) &&  (isspace(*scan))) scan++;
	if (*scan == 0) goto wrong_number_of_fields;

	regions = scan;
	while ((*scan!=0) && (!isspace(*scan))) scan++;
	while ((*scan!=0) &&  (isspace(*scan))) scan++;

	if (*scan == 0) goto wrong_number_of_fields;

	sizes = scan;
	while ((*scan!=0) && (!isspace(*scan))) scan++;
	while ((*scan!=0) &&  (isspace(*scan))) scan++;

	if (*scan == 0)
		adjacencies = NULL;
	else
		{
		adjacencies = scan;
		while ((*scan!=0) && (!isspace(*scan))) scan++;
		while ((*scan!=0) &&  (isspace(*scan))) scan++;
		}

	if (*scan != 0) goto wrong_number_of_fields;

	// parse the regions

	slashCol = -1;
	maxRgn   = 0;
	col = 0;
	for (scan=regions ; good_char(*scan) ; scan++)
		{
		if (*scan == '/')
			{
			if (slashCol >= 0) goto multiple_slashes;
			slashCol = col;
			continue;
			}
		if (col >= N) goto too_many_regions;
		if (*scan < 'A') goto bad_region;
		rgn = *scan - 'A';
		if (rgn >= N) goto bad_region;
		if (rgn > maxRgn) goto region_skipped;
		u->regions[col++] = rgn;
		if (rgn > maxRgn-1) maxRgn = rgn+1;
		}

	if      (slashCol == -1) u->col = 0;
	else if (slashCol == N)  u->col = 0;
	                    else u->col = slashCol;

	maxRgn--;

	// parse the sizes

	scan = sizes;

	for (rgn=0 ; rgn<=maxRgn ; rgn++)
		{
		if (bad_char(*scan)) goto bad_size;
		if (rgn != 0)
			{ if (*scan != ',') goto expected_size_comma;  scan++; }

		if (bad_char(*scan)) goto bad_size;
		if (*scan != 'A'+rgn) goto bad_size_region;
		scan++;

		size = 0;
		if (bad_char(*scan)) goto bad_size;
		if (!isdigit(*scan)) goto bad_size;

		size = *(scan++) - '0';
		if ((*scan!=0) && (isdigit(*scan))) size = 10*size + (*(scan++) - '0');
		if ((P != 0) && (size > P)) goto region_too_big;
		else if (size > Q)          goto region_too_big;
		u->sizeOf[rgn] = size;
		}

	// parse the adjacencies

	clear_adjacencies (u);

	if (adjacencies != NULL)
		{
		needComma = false;
		for (scan=adjacencies ; good_char(*scan) ; )
			{
			if (!needComma)
				needComma = true;
			else
				{ if (*scan != ',') goto expected_adj_comma;  scan++; }

			if (bad_char(*scan)) goto bad_adj;
			if (*scan < 'A') goto bad_adj_region;
			rgn1 = *scan - 'A';
			if (rgn1 >= N) goto bad_adj_region;
			scan++;

			if (bad_char(*scan)) goto bad_adj;
			if (*scan < 'A') goto bad_adj_region;
			rgn2 = *scan - 'A';
			if (rgn2 >= N) goto bad_adj_region;
			scan++;

			if (u->adjacent[rgn1][rgn2]) goto duplicate_adj_pair;
			u->adjacent[rgn1][rgn2] = true;
			u->adjacent[rgn2][rgn1] = true;
			}
		}

	// success

	return;

	// failure

wrong_number_of_fields:
	fprintf (stderr, "wrong number of fields: \"%s\"\n", s);
	exit (1);

multiple_slashes:
	fprintf (stderr, "multiple slashes: \"%s\"\n", regions);
	exit (1);

too_many_regions:
	fprintf (stderr, "too many regions: \"%s\"\n", regions);
	exit (1);

bad_region:
	fprintf (stderr, "bad region: \"%s\"\n", regions);
	exit (1);

region_skipped:
	fprintf (stderr, "region skipped (at %c): \"%s\"\n", *scan, regions);
	exit (1);

bad_size:
	fprintf (stderr, "bad sizes: \"%s\"\n", sizes);
	exit (1);

expected_size_comma:
	fprintf (stderr, "expected comma in sizes: \"%s\"\n", sizes);
	exit (1);

bad_size_region:
	fprintf (stderr, "bad region in sizes: \"%s\"\n", sizes);
	exit (1);

region_too_big:
	fprintf (stderr, "region too big: \"%s\"\n", sizes);
	exit (1);

bad_adj:
	fprintf (stderr, "bad adjacencies: \"%s\"\n", adjacencies);
	exit (1);

expected_adj_comma:
	fprintf (stderr, "expected comma in adjacencies: \"%s\"\n", adjacencies);
	exit (1);

bad_adj_region:
	fprintf (stderr, "bad region in adjacencies: \"%s\"\n", adjacencies);
	exit (1);

duplicate_adj_pair:
	fprintf (stderr, "duplicate pair in adjacencies (at %c%c): \"%s\"\n",
	                 rgn1+'A', rgn2+'A', adjacencies);
	exit (1);
	}

//----------
//
// packedge_to_string, unpackedge_to_string--
//	Convert an edge state to a string.
//
//----------
//
// Arguments:
//	packedge*	e:	(pointer to) The edge to convert.
//	 -OR-
//	unpackedge*	u:	(pointer to) The edge to convert.
//
// Returns:
//	A string representing the edge state.  (see note 1)
//
//----------
//
// notes:
//
// (1)	The memory containing the returned string belongs to this routine, as
//		static memory.  There are only three such memory blocks, and they are
//		used on subsequent calls on a rotating basis.  When you make more than
//		three calls, the results of previous calls are clobbered.
//
//----------

#if ((P != 0) && (P >= 100))
#error ***** unpackedge_to_string assumes maxPiece < 100 *****
#endif

#define maxEdgeString (N*(1+4+2*(N-1)))


static char* packedge_to_string
   (packedge*	e)
	{
	unpackedge	u;

	unpack_edge (e, &u);
	return unpackedge_to_string (&u);
	}


static char* unpackedge_to_string
   (unpackedge*	u)
	{
	static char	 s1[maxEdgeString+1], s2[maxEdgeString+1], s3[maxEdgeString+1];
	static char* s = s3;
	char*	ss;
	int		rgn, rgn1, rgn2, maxRgn;
	int		col;
	int		firstAdjacency;
	u8		rgnSize;

	ss = s = (s == s1)? s2 : (s == s2)? s3 : s1;	// (ping pong)

	// write regions

	maxRgn = -1;

	for (col=0 ; col<N ; col++)
		{
		rgn = u->regions[col];
		if ((u->col != 0) && (col == u->col)) *(ss++) = '/';
		*(ss++) = 'A' + rgn;
		if (rgn > maxRgn)
			maxRgn = rgn;
		}

	// write sizes

	*(ss++) = ' ';

	for (rgn=0 ; rgn<=maxRgn ; rgn++)
		{
		if (rgn != 0) *(ss++) = ',';
		*(ss++) = 'A' + rgn;
		rgnSize = u->sizeOf[rgn];
		if (rgnSize >= 10) *(ss++) = '0' + (rgnSize / 10);
		*(ss++) = '0' + (rgnSize % 10);
		}

	// write adjacencies

	firstAdjacency = true;

	for (rgn1=0 ; rgn1<maxRgn ; rgn1++)
		{
		if (is_full(u->sizeOf[rgn1])) continue;
		for (rgn2=rgn1+1 ; rgn2<=maxRgn ; rgn2++)
			{
			if (is_full(u->sizeOf[rgn2])) continue;
			if (!u->adjacent[rgn1][rgn2]) continue;

			if (firstAdjacency)
				{
				firstAdjacency = false;
				*(ss++) = ' ';
				}
			else
				*(ss++) = ',';

			*(ss++) = 'A' + rgn1;
			*(ss++) = 'A' + rgn2;
			}
		}

	*(ss++) = 0;
	return s;
	}

//----------
//
// draw_packedge--
//	Draw an ascii picture of an edge state.
//
//----------
//
// Arguments:
//	FILE*		f:		The file to draw to.
//	packedge*	e:		(pointer to) The edge to draw.
//	char*		prefix:	A string to write at the beginning of each line.  This
//						.. can be NULL.
//
// Returns:
//	(nothing)
//
//----------
//
// Here's a typical drawing, for a state AAABCD/CEC:
//
//		+   +   +   +   +   +   +
//		| A   A   A | B | C | D
//		+   +   +   +   +   +   +   +   +   +
//		|                         C | E | C |
//		+   +   +   +   +   +   +   +   +   +
//
//----------

#if (N > 26)
#error ***** draw_unpackedge assumes numColumns <= 26 *****
#endif


static void draw_packedge
   (FILE*		f,
	packedge*	e,
	char*		prefix)
	{
	unpackedge	u;

	unpack_edge (e, &u);
	draw_unpackedge (f, &u, prefix);
	}


static void draw_unpackedge
   (FILE*		f,
	unpackedge*	u,
	char*		prefix)
	{
	u8			col, rgn, wRgn;

	if (prefix == NULL) prefix = "";

	// draw the northern part (if there is one)

	if (u->col != 0)
		{
		fprintf (f, "%s", prefix);
		for (col=0 ; col<u->col ; col++) fprintf (f, "+   ");
		fprintf (f, "+\n");

		wRgn = noRegion;
		fprintf (f, "%s", prefix);
		for (col=0 ; col<u->col ; col++)
			{
			rgn = u->regions[col];
			if (rgn != wRgn) fprintf (f, "| %c ", rgn+'A');
			            else fprintf (f, "  %c ", rgn+'A');
			wRgn = rgn;
			}
		fprintf (f, "\n");
		}

	// draw the southern part

	fprintf (f, "%s", prefix);
	for (col=0 ; col<N ; col++) fprintf (f, "+   ");
	fprintf (f, "+\n");

	fprintf (f, "%s", prefix);
	for (col=0 ; col<u->col ; col++)
		{
		if (col == 0) fprintf (f, "|   ");
		         else fprintf (f, "    ");
		}
	wRgn = (u->col==0)? noRegion : u->regions[u->col];
	for ( ; col<N ; col++)
		{
		rgn = u->regions[col];
		if (rgn != wRgn) fprintf (f, "| %c ", rgn+'A');
					else fprintf (f, "  %c ", rgn+'A');
		wRgn = rgn;
		}
	fprintf (f, "|\n");

	fprintf (f, "%s", prefix);
	for (col=0 ; col<N ; col++) fprintf (f, "+   ");
	fprintf (f, "+\n");
	}

//----------
//
// initialize_wave_hash--
//	Initialize the wave hash.
//
// We simply fill the entire data structure with zeros.  This clears the
// counts, and since zero is not a legitimate edge state, we've effectively
// emptied every key.
//
//----------
//
// Arguments:
//	(none)
//
// Returns:
//	(nothing)
//
//----------

static void initialize_wave_hash
   (void)
	{
	size_t waveHashBytes       = waveHashEntries * sizeof(waveentry);
	size_t wavePreviousBytes   = waveHashEntries * sizeof(ucount);
	size_t waveCurrentBytes    = waveHashEntries * sizeof(ucount);
#if (poolCacheBytes != 0)
	size_t waveExpansionsBytes = waveHashEntries * sizeof(edgelist*);
#endif // poolCacheBytes

	if (waveHash == NULL)
		{
		waveHash = (waveentry*) calloc (waveHashEntries, sizeof(waveentry));
		if (waveHash == NULL)
			{
			fprintf (stderr, "failed to allocate %d entries for wave keys (%s bytes)\n",
			                 waveHashEntries, int_to_string((s64) waveHashBytes));
			exit (1);
			}

		wavePrevious = (ucount*) calloc (waveHashEntries, sizeof(ucount));
		if (wavePrevious == NULL)
			{
			fprintf (stderr, "failed to allocate %d entries for wave previous counts (%s bytes)\n",
			                 waveHashEntries, int_to_string((s64) wavePreviousBytes));
			exit (1);
			}

		waveCurrent = (ucount*) calloc (waveHashEntries, sizeof(ucount));
		if (waveCurrent == NULL)
			{
			fprintf (stderr, "failed to allocate %d entries for wave current counts (%s bytes)\n",
			                 waveHashEntries, int_to_string((s64) waveCurrentBytes));
			exit (1);
			}

#if (poolCacheBytes != 0)
		waveExpansions = (edgelist**) calloc (waveHashEntries, sizeof(edgelist*));
		if (waveExpansions == NULL)
			{
			fprintf (stderr, "failed to allocate %d entries for wave expansions pointers (%s bytes)\n",
			                 waveHashEntries, int_to_string((s64) waveExpansionsBytes));
			exit (1);
			}

		expansionsPool = (char*) calloc ((size_t) poolCacheBytes, sizeof(char));
		if (expansionsPool == NULL)
			{
			fprintf (stderr, "failed to allocate expansions cache pool (%s bytes)\n",
			                 int_to_string((s64) poolCacheBytes));
			exit (1);
			}

		nextInPool      = expansionsPool;
		bytesLeftInPool = poolCacheBytes;
		entriesInPool   = 0;
#endif // poolCacheBytes
		}
	else
		{
		memset (waveHash,       0, waveHashBytes);
		memset (wavePrevious,   0, wavePreviousBytes); // $$$ (there is probably no
		memset (waveCurrent,    0, waveCurrentBytes);  //      .. need to erase counts)
#if (poolCacheBytes != 0)
		memset (waveExpansions, 0, waveExpansionsBytes);

		nextInPool      = expansionsPool;
		bytesLeftInPool = poolCacheBytes;
		entriesInPool   = 0;
#endif // poolCacheBytes
		}

	entriesInHash = 0;

	if ((dbgWaveHash) || (dbgWaveStats))
		{
#if (poolCacheBytes == 0)
		fprintf (stderr, "wave hash has %d entries available (%s/%s/%s bytes)\n",
		                 waveHashEntries,
		                 int_to_string((s64) waveHashBytes),
		                 int_to_string((s64) wavePreviousBytes),
		                 int_to_string((s64) waveCurrentBytes));
#else
		fprintf (stderr, "wave hash has %d entries available (%s/%s/%s/%s/%s bytes)\n",
		                 waveHashEntries,
		                 int_to_string((s64) waveHashBytes),
		                 int_to_string((s64) wavePreviousBytes),
		                 int_to_string((s64) waveCurrentBytes),
		                 int_to_string((s64) waveExpansionsBytes),
		                 int_to_string((s64) poolCacheBytes));
#endif // poolCacheBytes
		}
	}

//----------
//
// print_wave_hash--
//	Print the contents of the wave hash.
//
//----------
//
// Arguments:
//	FILE*	f:	The file to print to.
//
// Returns:
//	(nothing)
//
//----------

static void print_wave_hash
   (FILE*		f)
	{
	u32			h;
	waveentry*	w;
	packedge*	e;
	ucount		p, c;

	for (h=0 ; h<waveHashEntries ; h++)
		{
		w = &waveHash[h];
		e = &w->key;
		p = wavePrevious[h];
		c = waveCurrent[h];

		if (dbgWaveHash)
			{
			if (!empty_packedge(*e))
				fprintf (stderr, "wave[%08X] = %08X %08X %08X " ucountFmt "/" ucountFmt "\n",
				                 h, e->v[2], e->v[1], e->v[0], fmt_ucount(p), fmt_ucount(c));
			}

		if (empty_packedge(*e)) continue;

		fprintf (f, edgeFmt " " ucountFmt "/" ucountFmt "\n",
		            packedge_to_string(e), fmt_ucount(p), fmt_ucount(c));
		}
	}

//----------
//
// dump_wave--
//	Dump the contents of the wave hash to a file, for debugging.
//
//----------
//
// Arguments:
//	FILE*	f:			The file to dump to.
//	int		showClosed:	true  => show whether states are "closed"
//						false => don't
//
// Returns:
//	(nothing)
//
//----------

static void dump_wave
   (FILE*		f,
	int			showClosed)
	{
	u32			h;
	waveentry*	w;
	packedge*	e;
	unpackedge	u;
	char*		closed;

// $$$ this should indicate expansions

	for (h=0 ; h<waveHashEntries ; h++)
		{
		w = &waveHash[h];
		e = &w->key;

		if (!empty_packedge(*e))
			{
			unpack_edge (e, &u);
			closed = "";
			if ((showClosed) && (edge_is_closed (&u))) closed = " closed";
			fprintf (stderr, "  h[%08X] " ucountFmt " " ucountFmt " <" edgeFmt ">%s\n",
			                 h, fmt_ucount(wavePrevious[h]), fmt_ucount(waveCurrent[h]),
			                 unpackedge_to_string(&u), closed);
			}
		else if ((!is_zero(wavePrevious[h])) || (!is_zero(waveCurrent[h])))
			fprintf (stderr, "  h[%08X] " ucountFmt " " ucountFmt " (empty)\n",
			                 h, fmt_ucount(wavePrevious[h]), fmt_ucount(waveCurrent[h]));
		}

	}

//----------
//
// add_to_wave_hash--
//	Add an (edge,count) pair to the wave hash.
//
// If the edge isn't currently in the hash a new entry is created (with zero
// counts).  The count is added to the count for the current wave.
//
//----------
//
// Arguments:
//	packedge*	e:		(pointer to) The edge.
//	ucount		count:	The count to add.
//
// Returns:
//	(nothing)
//
//----------
//
// The technique used is a variant of cuckoo hashing.  We have three hash
// functions and two bins per hash value.  We truncate each hash functions down
// to an even value, and consider that entry and the next one as the two bins
// for that value.
//
// $$$ add more about cuckoo hashing and details.
//
//	... hash_func1 is quicker than hash_func2 but probably less random (though,
//	    since we are doing a mod operator, speed might not be that important
//	... we hope to find most entries with the quick hash function
//	... this is my first cucko hash implementation, it could undoubtedly be
//	    be improved
//
//----------

// $$$ figure out the correct way to const pointers like these

static void add_to_wave_hash
   (packedge*	e,
	ucount		count)
	{
	u32			h, h1, h2, h3, prevH, r;
	waveentry	hobo1, hobo2;
	waveentry*	hobo, *next;
	ucount		hoboP, hoboC, nextP, nextC;
	int			triesLeft;
#if (poolCacheBytes != 0)
	edgelist*	hoboX, *nextX;
#endif // poolCacheBytes

	if (dbgWaveHash)
		{
		h1 = hash_func1 (e);
		h2 = hash_func2 (e, h1);
		h3 = hash_func3 (e, h1, h2);

		fprintf (stderr, "==========\n");
		fprintf (stderr, "adding " ucountFmt " ways for <" edgeFmt ">\n",
		                 fmt_ucount(count), packedge_to_string(e));
		fprintf (stderr, "  e = %08X %08X %08X\n", e->v[2], e->v[1], e->v[0]);
		fprintf (stderr, "  h = %08X %08X %08X\n", h1, h2, h3);
		}

	// does this edge already exist in the hash?

	h = h1 = hash_func1 (e);
	if (equal_packedge(waveHash[h].key,*e)) goto entry_found;
	h++;
	if (equal_packedge(waveHash[h].key,*e)) goto entry_found;

	h = h2 = hash_func2 (e, h1);
	if (equal_packedge(waveHash[h].key,*e)) goto entry_found;
	h++;
	if (equal_packedge(waveHash[h].key,*e)) goto entry_found;

	h = h3 = hash_func3 (e, h1, h2);
	if (equal_packedge(waveHash[h].key,*e)) goto entry_found;
	h++;
	if (equal_packedge(waveHash[h].key,*e)) goto entry_found;

	goto entry_not_found;

	// we found this edge in the hash;  add to the current count

entry_found:

	if (dbgWaveHash)
		{
		ucount newCurrent = waveCurrent[h];
		accumulate(newCurrent,count);
		fprintf (stderr, "  found at %08X, current=" ucountFmt " (was " ucountFmt ")\n",
		                 h, fmt_ucount(newCurrent), fmt_ucount(waveCurrent[h]));
		}

	accumulate(waveCurrent[h],count);
	return;

	// this edge doesn't exist in the hash;  see if any of its bins are empty

entry_not_found:

	hobo1.key = *e;
	hoboP     = to_ucount(0);
	hoboC     = count;
#if (poolCacheBytes != 0)
	hoboX     = NULL;
#endif // poolCacheBytes
	hobo      = &hobo1;
	next      = &hobo2;

	h = h1;  if (empty_packedge(waveHash[h].key)) goto no_shuffling_needed;
	h++;     if (empty_packedge(waveHash[h].key)) goto no_shuffling_needed;
	h = h2;  if (empty_packedge(waveHash[h].key)) goto no_shuffling_needed;
	h++;     if (empty_packedge(waveHash[h].key)) goto no_shuffling_needed;
	h = h3;  if (empty_packedge(waveHash[h].key)) goto no_shuffling_needed;
	h++;     if (empty_packedge(waveHash[h].key)) goto no_shuffling_needed;

	// this edge doesn't exist in the hash and all its bins are in use; we
	// have to move entries around to make room for it

	if (dbgWaveHash)
		fprintf (stderr, "  all 6 bins occupied\n");

	prevH = noHashIndex;

	for (triesLeft=waveHashMaxTries ; triesLeft-->0 ; )
		{
		// choose one of the six bins, but excluding the bin-pair we just
		// pulled the transient from

		if (prevH == noHashIndex)
			{
			r = urand32 (0, 5);
			if      ((r & 3) == 0) h = h1 + (r & 1);
			else if ((r & 3) == 1) h = h2 + (r & 1);
			else                   h = h3 + (r & 1);
			}
		else if ((prevH & (~1)) == h1)
			{
			r = urand32 (0, 3);
			if ((r & 2) == 0) h = h2 + (r & 1);
						 else h = h3 + (r & 1);
			}
		else if ((prevH & (~1)) == h2)
			{
			r = urand32 (0, 3);
			if ((r & 2) == 0) h = h1 + (r & 1);
						 else h = h3 + (r & 1);
			}
		else // if ((prevH & (~1)) == h3)
			{
			r = urand32 (0, 3);
			if ((r & 2) == 0) h = h1 + (r & 1);
						 else h = h2 + (r & 1);
			}

		prevH = h;

		// swap the the transient with the entry from that bin

		*next = waveHash[h];  waveHash[h] = *hobo;
		if (hobo == &hobo1) { hobo = &hobo2;  next = &hobo1; }
		               else { hobo = &hobo1;  next = &hobo2; }

		nextP = wavePrevious  [h];  wavePrevious  [h] = hoboP;  hoboP = nextP;
		nextC = waveCurrent   [h];  waveCurrent   [h] = hoboC;  hoboC = nextC;
#if (poolCacheBytes != 0)
		nextX = waveExpansions[h];  waveExpansions[h] = hoboX;  hoboX = nextX;
#endif // poolCacheBytes

		if (dbgWaveHash)
			{
			if (triesLeft == waveHashMaxTries-1)
				fprintf (stderr, "  h[%08X] <- <" edgeFmt ">, current=" ucountFmt " (shuffling)\n",
				                 h, packedge_to_string(&waveHash[h].key), fmt_ucount(waveCurrent[h]));
			else
				fprintf (stderr, "  h[%08X] <- <" edgeFmt "> (shuffling)\n",
				                 h, packedge_to_string(&waveHash[h].key));
			}

		// compute the hash functions for the new transient

		h = h1 = hash_func1 (&hobo->key);
		if (empty_packedge(waveHash[h].key)) goto shuffling_successful;
		h++;
		if (empty_packedge(waveHash[h].key)) goto shuffling_successful;

		h = h2 = hash_func2 (&hobo->key, h1);
		if (empty_packedge(waveHash[h].key)) goto shuffling_successful;
		h++;
		if (empty_packedge(waveHash[h].key)) goto shuffling_successful;

		h = h3 = hash_func3 (&hobo->key, h1, h2);
		if (empty_packedge(waveHash[h].key)) goto shuffling_successful;
		h++;
		if (empty_packedge(waveHash[h].key)) goto shuffling_successful;
		}

	// we couldn't find a place to put the displaced edge

	fprintf (stderr, "cuckoo hash shuffling failed, with %d of %d entries used (%.1f%%)\n",
	                 entriesInHash, waveHashEntries,
	                 (entriesInHash*100.0)/waveHashEntries);
	exit (1);

	// we found a place to put the new or displaced edge

no_shuffling_needed:

	if (dbgWaveHash)
		fprintf (stderr, "  h[%08X] <- <" edgeFmt ">, current=" ucountFmt " (bin was empty)\n",
		                 h, packedge_to_string(e), fmt_ucount(hoboC));
	goto write_entry;

shuffling_successful:
	if (dbgWaveHash)
		fprintf (stderr, "  h[%08X] <- <" edgeFmt "> (shuffle complete)\n",
		                 h, packedge_to_string(&hobo->key));
	goto write_entry;

write_entry:
	waveHash      [h] = *hobo;
	wavePrevious  [h] = hoboP;
	waveCurrent   [h] = hoboC;
#if (poolCacheBytes != 0)
	waveExpansions[h] = hoboX;
#endif // poolCacheBytes
	entriesInHash++;

	if (dbgWaveHash)
		fprintf (stderr, "  %d items in hash\n", entriesInHash);
	}

//----------
//
// wave_hash_previous--
//	Find an edge count in the wave hash.
//
// If the edge isn't currently in the hash we consider the count to be zero.
//
//----------
//
// Arguments:
//	packedge*	e:	(pointer to) The edge.
//
// Returns:
//	The previous count associated with that edge.
//
//----------

static ucount wave_hash_previous
   (packedge*	e)
	{
	u32			h, h1, h2;

	// does this edge already exist in the hash?

	h = h1 = hash_func1 (e);
	if (equal_packedge(waveHash[h].key,*e)) return wavePrevious[h];
	h++;
	if (equal_packedge(waveHash[h].key,*e)) return wavePrevious[h];

	h = h2 = hash_func2 (e, h1);
	if (equal_packedge(waveHash[h].key,*e)) return wavePrevious[h];
	h++;
	if (equal_packedge(waveHash[h].key,*e)) return wavePrevious[h];

	h = hash_func3 (e, h1, h2);
	if (equal_packedge(waveHash[h].key,*e)) return wavePrevious[h];
	h++;
	if (equal_packedge(waveHash[h].key,*e)) return wavePrevious[h];

	// this edge doesn't exist in the hash, so the previous count is zero

	return to_ucount(0);
	}

//----------
//
// current_to_previous--
//	Move all edge counts in the wave hash to from current to previous, and
//	clear the current counts.
//
//----------
//
// Arguments:
//	(none)
//
// Returns:
//	The total of what were the current counts in the hash.
//
//----------

static ucount current_to_previous
   (void)
	{
	u32			h;
	waveentry*	w;
	packedge*	e;
	ucount		count, current;

	count = to_ucount(0);
	for (h=0 ; h<waveHashEntries ; h++)
		{
		w = &waveHash[h];
		e = &w->key;
		if (empty_packedge(*e)) continue;

		wavePrevious[h] = current = waveCurrent[h];
		waveCurrent [h] = to_ucount(0);
		accumulate(count,current);
		}

	return count;
	}

//----------
//
// next_in_current_wave, next_in_previous_wave--
//	Report, in successive calls, all active edges in the wave.
//
//----------
//
// Arguments:
//	u32*	h:			(pointer to) The previous hash bin we reported.  On the
//						.. first call, this should be preHashIndex.  This is
//						.. updated upon return, to point at the hash bin
//						.. corresonding to the edge we return.
//	ucount*	count:		(pointer to) Place to return the next edge's count.
//						.. This may be NULL.
//	int		clearAsWeGo:(for next_in_previous_wave only) See note (1).
//
// Returns:
//	A pointer to the next edge, in the wave, for which the specified count is
//	non-zero.  If there are no more such edges, we return NULL.
//
//----------
//
// Typical calling sequence:
//
//		u32			h;
//		edge*		e;
//		ucount		count;
//		waveentry*	w;
//
//		h = preHashIndex;
//		while ((e = next_in_current_wave(&h,&count)) != NULL)
//			{
//			w = &waveHash[h];
//
//			... do something with e and/or w
//			}
//
//----------
//
// notes:
//
// (1)	The clearAsWeGo argument serves two purposes.  If this is false, we
//		simply scan the hash once.  This is suitable for when the hash is not
//		fluid (not actively having things added to it).
//
//		However, when the hash IS fluid, a simple scan can fail.  The problem
//		occurs when the addition of a new item causes an existing item to be
//		shuffled 'behind' the current scan location (e.g. when the scan has
//		reached entry 100, the item at entry 200 is displaces (by a new item)
//		and moved to entry 50.
//
//		To handle this case, set clearAsWeGo to true.  This has two effects.
//		First, as each entry is reported, its count is erased.  Second, we will
//		make multiple passes through the hash, until we make a pass for which
//		no entries are reported (all the counts are zero).
//
//----------

static packedge* next_in_current_wave
   (u32*		_h,
	ucount*		_count)
	{
	u32			h = *_h;
	waveentry*	w = NULL;
	packedge*	e = NULL;
	ucount		count = to_ucount(0);

	if      (h == preHashIndex)    h = 0;
	else if (h == noHashIndex)     return NULL;
	else if (h >= waveHashEntries) { *_h = noHashIndex;  return NULL; }
	else                           h++;

	// scan ahead until we find the next non-empty or non-zero entry, or
	// reach the end

	for ( ; h<waveHashEntries ; h++)
		{
		w = &waveHash[h];
		count = waveCurrent[h];
		if (is_zero(count)) continue;
		e = &w->key;
		if (!empty_packedge(*e)) break;
		}

	// since the hash is presumed not to be fluid, one pass is always enough

	if (h >= waveHashEntries)
		{ *_h = noHashIndex;  return NULL; }

	*_h = h;
	if (_count != NULL) *_count = count;
	return e;
	}


static packedge* next_in_previous_wave
   (u32*		_h,
	ucount*		_count,
	int			clearAsWeGo)
	{
	u32			h = *_h;
	waveentry*	w = NULL;
	packedge*	e = NULL;
	ucount		count = to_ucount(0);

	if      (h == preHashIndex)    h = 0;
	else if (h == noHashIndex)     return NULL;
	else if (h >= waveHashEntries) { *_h = noHashIndex;  return NULL; }
	else                           h++;

	// scan the hash until we find a non-zero entry, making multiple passes if
	// necessary

	while (true)
		{
		// scan ahead until we find the next non-empty or non-zero entry, or
		// reach the end

		for ( ; h<waveHashEntries ; h++)
			{
			w = &waveHash[h];
			count = wavePrevious[h];
			if (is_zero(count)) continue;
			if (dbgWaveHash)
				fprintf (stderr, "wave[%08X].previous is " ucountFmt "\n", h, fmt_ucount(count));
			e = &w->key;
			if (!empty_packedge(*e)) break;
			}

		// if the hash is not fluid, one pass is always enough

		if (!clearAsWeGo)
			{
			if (h >= waveHashEntries)
				{ *_h = noHashIndex;  return NULL; }

			*_h = h;
			if (_count != NULL) *_count = count;
			return e;
			}

		// otherwise, the hash is fluid;  if we found a non-zero entry, clear
		// it and return

		if (h < waveHashEntries)
			{
			*_h = h;
			wavePrevious[h] = to_ucount(0);
			if (_count != NULL) *_count = count;
			return e;
			}

		// otherwise, the hash is fluid and we found no entry;  if we completed
		// a full pass without finding anything, the whole process is complete

		if (*_h == preHashIndex)
			{ *_h = noHashIndex;  return NULL; }


		// otherwise, we need to make another pass

		*_h = preHashIndex;
		h   = 0;
		}
	}

//----------
//
// hash_func1, hash_func2, hash_func3--
//	Compute an edge's hash value.
//
// hash_func2 is an endian dependent variation of variant of Austin Appleby's
// MurmurHash2.
//
// hash_func3 is an endian dependent variation of variant of Paul Hsieh's
// Superfast Hash.
//
//----------
//
// Arguments:
//	packedge*	e:			(pointer to) The edge.
//	u32			prohibited:	(for hash_func2 and hash_func3) A value we are not
//							.. allowed to produce.
//	u32			prohibited2:(for hash_func3 only) Another value we are not
//							.. allowed to produce.  prohibited2 != prohibited.
//
// Returns:
//	The edge's hash value, an even number in the range 0..waveHashEntries-1.
//
//----------

#if (packedgeChunks != 3)
#error ***** hash_func1 assumes packedgeChunks == 3 *****
#endif


static u32 hash_func1
   (packedge*	e)
	{
	u32			h;

	h = (e->v[0] + e->v[1]) - e->v[2];
	return (h % waveHashEntries) & (~1);
	}


static u32 hash_func2
   (packedge*	e,
	u32			prohibited)
	{
	const u32	seed = 0x5C3FC4D3;
	const u32	m    = 0x87C10417;
	const int	r    = 24;
	const u32*	data = ((const u32*) e->v) + packedgeChunks;
	u32			len, h, k;

	h = seed ^ packedgeChunks;
	for (len=packedgeChunks ; len>0 ; len--)
		{
		k = *(--data);  k *= m;  k ^= k >> r;  k *= m;
		h *= m;  h ^= k;
		}

	h ^= h >> 13;  h *= m;  h ^= h >> 15;

	h = (h % (waveHashEntries-2)) & (~1);
	if (h >= prohibited) h += 2;
	return h;
	}


static u32 hash_func3
   (packedge*	e,
	u32			prohibited,
	u32			prohibited2)
	{
	u16*		data = (u16*) e;
	int			len;
	u32			h;

	h = sizeof(packedge);
	for (len=packedgeChunks ; len>0 ; len--)
		{
		h += *(data++);
		h =  (h << 16) ^ ((*(data++) << 11) ^ h);
		h += h >> 11;
		}

	h ^= h << 3;
	h += h >> 5;
	h ^= h << 4;
	h += h >> 17;
	h ^= h << 25;
	h += h >> 6;

	if (prohibited < prohibited2)
		{ u32 t = prohibited;  prohibited = prohibited2;  prohibited2 = t; }

	h = (h % (waveHashEntries-4)) & (~1);
	if (h >= prohibited)  h += 2;
	if (h >= prohibited2) h += 2;
	return h;
	}

//----------
//
// add_expansions_to_pool--
//	Add a list of expansion states to the expansions pool.
//
//----------
//
// Arguments:
//	edgelist*	expansions:	(Pointer to) the list of expansion states.
//
// Returns:
//	A pointer into the expansions pool, to where the list was added.  NULL
//	indicates they were not added to the pool.
//
//----------

#if (poolCacheBytes != 0)

static edgelist* add_expansions_to_pool
   (edgelist*	expansions)
	{
	u32			bytesNeeded = sizeof_edgelist(expansions->len);
	edgelist*	inPool;

	if (bytesNeeded > bytesLeftInPool) return NULL;

	inPool = (edgelist*) nextInPool;
	nextInPool      += bytesNeeded;
	bytesLeftInPool -= bytesNeeded;
	entriesInPool++;

	memcpy (inPool, expansions, bytesNeeded);

	return inPool;
	}

#endif // poolCacheBytes

//----------
//
// reverse_bits--
//	Reverse the order of bits in a word.
//
//----------
//
// Arguments:
//	int		numBits:	The number of bits in the word.
//	u32		bits:		The word.
//
// Returns:
//	A word with the bits in reverse order.
//
//----------

static u32 reverse_bits
   (int		numBits,
	u32		bits)
	{
	u32		bit, revBit, revBits;
	int		i;

	revBits = 0;
	for (i=0, bit=1,   revBit=(1<<(numBits-1)) ;
	     i<numBits ;
	     i++, bit<<=1, revBit>>=1)
		{ if ((bits & bit) != 0) revBits += revBit; }

	return revBits;
	}

//----------
//
// int_to_string--
//	Convert an integer to a string, in units of K, M, or G.
//
//----------
//
// Arguments:
//	s64		v:	The number.
//
// Returns:
//	A string representing the number.  (see note 1)
//
//----------
//
// notes:
//
// (1)	The memory containing the returned string belongs to this routine, as
//		static memory.  There are only four such memory blocks, and they are
//		used on subsequent calls on a rotating basis.  When you make more than
//		four calls, the results of previous calls are clobbered.
//
// (2)	In agreement with the meaning of unit suffixes described at
//		http://en.wikipedia.org/wiki/Petabyte, the units multiplier is 1000
//		(not 1024).
//
//----------

// SI unit prefixes (see, e.g., http://en.wikipedia.org/wiki/SI_prefix)

char* unitName[] = { "", "K", "M", "G", "T", "P", "E", "Z" };


static char* int_to_string
   (s64		v)
	{
	static char	 s1[12], s2[12], s3[12], s4[12], s5[12];
	static char* s = s5;
	int		sign, unit;
	s64		vv;
	float	rep;

	s = (s == s1)? s2 : (s == s2)? s3 : (s == s3)? s4 : (s == s4)? s5 : s1; // (ping pong)

	// convert

	if (v >= 0) { sign = '\0';  vv = v;  }
	       else { sign = '-';   vv = -v; }

	unit = 0;
	for (rep=vv ; vv>999 ; vv/=1000,rep/=1000)
		unit++;

	if (rep > 999) { rep /= 1024;  unit++; }

	if (sign < 0) sprintf (s, "-%.1f%s", rep, unitName[unit]);
	         else sprintf (s,  "%.1f%s", rep, unitName[unit]);

	return s;
	}

//----------
//
// bits_string--
//	Convert a value to a string, as binary.
//
//----------
//
// Arguments:
//	int		numBits:	The number of bits in the word.
//	u64		bits:		The word.
//
// Returns:
//	A string representing the binary value.  (see note 1)
//
//----------
//
// notes:
//
// (1)	The memory containing the returned string belongs to this routine, as
//		static memory.  There are only four such memory blocks, and they are
//		used on subsequent calls on a rotating basis.  When you make more than
//		four calls, the results of previous calls are clobbered.
//
//----------

static char* bits_string
   (int		numBits,
	u64		bits)
	{
	static char	 s1[65], s2[65], s3[65], s4[65];
	static char* s = s4;
	char*	ss;
	u32		bit;
	int		i;

	ss = s = (s == s1)? s2 : (s == s2)? s3 : (s == s3)? s4 : s1; // (ping pong)

	// convert

	for (i=0, bit=(1<<(numBits-1)) ; i<numBits ; i++, bit>>=1)
		{
		if ((bits & bit) != 0) *(ss++) = '1';
		                  else *(ss++) = '0';
		}

	*(ss++) = 0;
	return s;
	}

//----------
//
// seconds_to_string--
//	Convert a count of seconds to a string, in hours, minutes, seconds.
//
//----------
//
// Arguments:
//	float	seconds:	The number of seconds.
//
// Returns:
//	A string representing the seconds.  (see note 1)
//
//----------
//
// notes:
//
// (1)	The memory containing the returned string belongs to this routine, as
//		static memory.  There are only four such memory blocks, and they are
//		used on subsequent calls on a rotating basis.  When you make more than
//		four calls, the results of previous calls are clobbered.
//
//----------

static char* seconds_to_string
   (float	_seconds)
	{
	static char	 s1[31], s2[31], s3[31], s4[31];
	static char* s = s4;
	char*	sign;
	float	deciSeconds, seconds;
	u32		days, hours, minutes;

	s = (s == s1)? s2 : (s == s2)? s3 : (s == s3)? s4 : s1; // (ping pong)

	// round seconds to nearest tenths, so that the string doesn't end up with
	// something silly like "3m60.0s"

	if (_seconds < 0) { sign = "-";  seconds = -_seconds; }
	             else { sign = "";   seconds =  _seconds; }

	deciSeconds = floor (seconds * 10 + 0.5);

	// convert

	minutes =  (int) (deciSeconds / 600);
	deciSeconds -= 600 * minutes;
	seconds     =  deciSeconds / 10;

	hours   =  minutes / 60;
	minutes -= 60 * hours;

	days    =  hours / 24;
	hours   -= 24 * days;

	if (days > 0)
		sprintf (s, "%s" "%ud%02uh%02um%04.1fs", sign, days, hours, minutes, seconds);
	else if (hours > 0)
		sprintf (s, "%s"      "%uh%02um%04.1fs", sign,       hours, minutes, seconds);
	else if (minutes > 0)
		sprintf (s, "%s"           "%um%04.1fs", sign,              minutes, seconds);
	else
		sprintf (s, "%s"                "%.1fs", sign,                       seconds);

	return s;
	}

//----------
//
// ucount_to_string--
//	Convert a ucount to a string.
//
//----------
//
// Arguments:
//	ucount*		v:	(pointer to) The number.
//
// Returns:
//	A string representing the number.  (see note 1)
//
//----------
//
// notes:
//
// (1)	The memory containing the returned string belongs to this routine, as
//		static memory.  There are only four such memory blocks, and they are
//		used on subsequent calls on a rotating basis.  When you make more than
//		four calls, the results of previous calls are clobbered.
//
//----------

#if (countBits > 96)
#error ***** ucount_to_string doesn't support countBits bigger than 96 *****
#endif // countBits

#if (countBits > 64)

static char* ucount_to_string
   (ucount*		v)
	{
	static char	 s1[81], s2[81], s3[81], s4[81];
	static char* s = s4;
	u64		base, v3, v2, v1, t;

	s = (s == s1)? s2 : (s == s2)? s3 : (s == s3)? s4 : s1; // (ping pong)

	if (v->ms == 0)
		{ sprintf (s, "%ju", v->ls);  return s; }

	// split the ls part of the number into three decimal pieces, base 10^9;
	// this value is chosen because it is less than 2^32;  being less than 2^32
	// means we can hold two digits in a 64-bit int

	base = 1000000000LL;

	v2 = v->ls / base;  v3 = v->ls - (v2 * base);
	v1 = v2    / base;  v2 = v2    - (v1 * base);

	// add in the ms part of the number;  in this base,
	//   2^64 =          18,446744073,709551616
	//   2^96 = 79228162514,264337593,543950336
	// note that we allow more than 9 digits in the ms piece, because we have
	// no risk of overflow there

	                                      // (===== proof of no overflow =====)
	v3 += v->ms * 709551616LL;            // (max = 3047500985834398719 < 2^62)
	t  =  v3 / base;  v3 = v3 - (t * base);
	v2 += t + (v->ms * 446744073LL);      // (max = 1918751186817593519 < 2^61)
	t  =  v2 / base;  v2 = v2 - (t * base);
	v1 += t + (v->ms * 18LL);             // (max =        156537573824 < 2^38)

	// convert the number to a string

	if      (v1 > 0) sprintf (s, "%ju%09ju%09ju", v1, v2, v3);
	else if (v2 > 0) sprintf (s,      "%ju%09ju",     v2, v3);
	else             sprintf (s,           "%ju",         v3);

	return s;
	}

#endif // countBits

//----------
//
// ucount_accumulation--
//	Add one ucount to another, accumulating the result.
//
//----------
//
// Arguments:
//	ucount*		sum:	(pointer to) The number to add to.
//	ucount*		v:		(pointer to) The number to add.
//
// Returns:
//	(nothing)
//
//----------

#if (countBits > 64)

static void ucount_accumulation
   (ucount*		sum,
	ucount*		v)
	{
	u64			oldMs, oldLs, newLs;

	oldMs = sum->ms;
	oldLs = sum->ls;

	sum->ls = newLs = sum->ls + v->ls;
	if (newLs < oldLs) sum->ms += v->ms + 1;   // carry
	              else sum->ms += v->ms;       // no carry
	}

#endif // countBits

//----------
//
// val_to_ucount--
//	Convert a small number to a ucount.
//
//----------
//
// Arguments:
//	u64		v:	The number.
//
// Returns:
//	The number, as a ucount.
//
//----------

#if (countBits > 64)

static ucount val_to_ucount
    (u64	v)
	{
	ucount	u;

	memset (&u, 0, sizeof(u));
	u.ls = v;

	return u;
	}

#endif // countBits

//----------
//
// ucount_is_zero--
//	Determine if a ucount is zero.
//
//----------
//
// Arguments:
//	ucount*		v:	(pointer to) The number.
//
// Returns:
//	true if the number is zero;  false if it is non-zero.
//
//----------

#if (countBits > 64)

static int ucount_is_zero
   (ucount*		v)
	{
	if (v->ls != 0) return false;
	if (v->ms != 0) return false;
	return true;
	}

#endif // countBits
