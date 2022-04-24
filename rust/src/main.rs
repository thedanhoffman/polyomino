const MAX_PIECE: i32 = 5;
const MIN_PIECE: i32 = 5;
const NUM_ROWS: usize = 5;
const NUM_COLS: usize = 5;
const COLUMN_STEP: i32 = NUM_COLS as i32;
const CACHE_LIMIT: i32 = 0;
const BACKTRACK: bool = false;
const PROGRESS: bool = false;

#[derive(Eq, PartialEq, Hash)]
struct PolyominoGridEdgeStamp {
    col: i32,
    regions: [i8; NUM_COLS as usize],
    size_of: [i32; (MAX_PIECE * MIN_PIECE) as usize],
    adjacent: [bool; (MAX_PIECE * MIN_PIECE) as usize],

    stamp_val: Option<i32>
}

#[derive(Eq, PartialEq, Hash)]
struct PolyominoGridEdge {
    // from __init__
    pges: Option<PolyominoGridEdgeStamp>,

    // from set_space
    stamp_bits: i32,
    piece_bits: i32,
}

impl PolyominoGridEdge {
    fn new(
        pges: Option<PolyominoGridEdgeStamp>
    ) -> Self {
        Self {
            stamp_bits: if NUM_COLS < 4 {
                2
            } else if NUM_COLS < 8 {
                3
            } else if NUM_COLS < 16 {
                4
            } else {
                5
            },
            piece_bits: if MAX_PIECE < 4 {
                2
            } else if MAX_PIECE < 8 {
                3
            } else if MAX_PIECE < 16 {
                4
            } else {
                5
            },
            pges
        }
    }

    fn from_stamp() -> Self {
        todo!()
    }

    fn stamp(&self) -> i32 {
        self.be_stamped()
    }

    fn be_stamped(&self) -> i32 {
        if let Some(ref pges) = self.pges.as_ref() {
            let mut rgns = Vec::new();
            for rgn in pges.regions.iter() {
                if !rgns.contains(rgn) {
                    rgns.push(rgn.clone());
                }
            }
            rgns.sort();
            
            let mut short_regions = pges.size_of.iter().filter(|rgn| {
                pges.size_of[**rgn as usize] < MAX_PIECE
            }).collect::<Vec<_>>();
           
            short_regions.sort();

            let b = self.stamp_bits;
            let p = self.piece_bits;
            let mut stamp: i32 = 0;

            // encode adjacencies
            // NOTE: this has a negative index in the beginning of the for loop, that's
            // obviously wrong but not sure hwo to fix that...

            if short_regions.len() >= 2 {
            for ix in (0 as i64..(short_regions.len() - 2) as i64).rev() {
                // the python code does this with xrange but this should be the same range, right?
                let rgn1 = short_regions[ix as usize];
                for iy in (ix as i64..(short_regions.len() - 1) as i64).rev() {
                    let rgn2 = short_regions[iy as usize];
                    stamp <<= 1;
                    if pges.adjacent[(rgn1 + rgn2) as usize] { // ???
                        stamp += 1;
                    }
                }
            }
            }

            // encode sizes
            for ix in (0 as i64..(rgns.len() - 1) as i64).rev() {
                stamp = (stamp << p) + pges.size_of[rgns[ix as usize] as usize];
            }

            // encode regions
            for ix in (0..NUM_COLS-1).rev() {
                stamp = (stamp << b) + pges.regions[ix] as i32;
            }

            // encode col
            stamp = (stamp << b) + pges.col;

            stamp
        } else {
            unreachable!()
        }
    }
}

fn main() {
    assert![MIN_PIECE >= 2];
    assert![MAX_PIECE >= MIN_PIECE];
    assert![NUM_COLS >= 1 && NUM_COLS <= 25];

    let mut pge = PolyominoGridEdge::new(
        None
    );

    #[derive(Default)]
    struct PolyominoStateToExpansion {};

    let mut state_to_expansion: [PolyominoStateToExpansion; NUM_COLS];
    
    let mut num_cached_states = 0;
    let mut expansion_cnt = 0;
    let mut expansion_len = 0;

    let mut initial_states = {
        let mut num_walls = NUM_COLS - 1;
        let mut state_to_ways = std::collections::HashMap::new();

        for mut wall_bits in 0..(1 << num_walls) {
            let rev_bits = {
                (0..num_walls).map(|i| if wall_bits & (1 << i) != 0 {
                    1 << (num_walls - 1 - i)
                } else {
                    0
                }).sum::<i32>()
            };

            let ways = if rev_bits > wall_bits {
                continue;
            } else if rev_bits == wall_bits {
                1
            } else {
                2
            };

            let mut regions = [0; NUM_COLS];
            let mut size_of = [0; (MAX_PIECE * MIN_PIECE) as usize];
            let mut adjacent = [false; (MAX_PIECE * MIN_PIECE) as usize];

            let mut num = 0;
            let mut rgn = num as i8;
            regions[0] = rgn;
            size_of[rgn as usize] = 1;
            
            let mut region_too_big = false;
        
            for col in 1..NUM_COLS {
                if wall_bits & 1 == 0 {
                    regions[col] = rgn;
                    size_of[rgn as usize] += 1;
                    if size_of[rgn as usize] > MAX_PIECE {
                        region_too_big = true;
                        break;
                    }
                } else {
                    let w = rgn;
                    num += 1;
                    rgn = num as i8;
                    regions[col] = rgn;
                    size_of[rgn as usize] = 1;
                    // WAIT A MINUTE this is as-written in the Python code, are
                    // they using addition as a non-commutative operator???
                   
                    let rgn_w_idx = (rgn + w) as usize;
                    adjacent[rgn_w_idx] = true;
                }

                wall_bits >>= 1;
            }

            if region_too_big {
                continue;
            }

            let mut state = PolyominoGridEdge::new(
                Some(PolyominoGridEdgeStamp {
                    col: 0,
                    regions, size_of, adjacent,
                    stamp_val: None
                })
            );

            // NOTE: might be worth it to fix this line...
            assert![state_to_ways.contains_key(&state.stamp()) == false];
            
            state_to_ways.insert(state.stamp(), ways);
        }

        state_to_ways
    };

    println!("initial states: {:#?}", &initial_states);
}
