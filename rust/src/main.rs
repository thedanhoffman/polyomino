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
    regions: Vec<i8>,
    size_of: [i32; (MAX_PIECE * MIN_PIECE) as usize],
    adjacent: [bool; (MAX_PIECE * MIN_PIECE) as usize]
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

    fn stamp(&mut self) -> ! {
        //if !self.stamped {
        if true {
            self.be_stamped();
        }
        todo!()
    }

    fn be_stamped(&mut self) -> ! {
        if let Some(ref mut pges) = self.pges.as_mut() {
            let mut rgns = Vec::new();
            for rgn in pges.regions.iter() {
                if !rgns.contains(rgn) {
                    rgns.push(rgn.clone());
                }
            }
            rgns.sort();
            
            let mut short_regions = pges.size_of.iter().filter(|rgn| {
                pges.size_of[**rgn as usize] >= MAX_PIECE
            }).collect::<Vec<_>>();
           
            short_regions.sort();

            let b = self.stamp_bits;
            let p = self.piece_bits;
            let mut stamp = 0;

            // encode adjacencies
            for ix in (-1 as i64..(short_regions.len() - 2) as i64).rev() {
                // the python code does this with xrange but this should be the same range, right?
                let rgn1 = short_regions[ix as usize];
                for iy in (-1 as i64..(short_regions.len() - 1) as i64).rev() {
                    let rgn2 = short_regions[iy as usize];
                    stamp <<= 1;
                    if pges.adjacent[(rgn1 + rgn2) as usize] { // ???
                        stamp += 1;
                    }
                }
            }

            todo!()
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

            let mut regions: Vec<i8> = Vec::new();
            let mut size_of = [0; (MAX_PIECE * MIN_PIECE) as usize];
            let mut adjacent = [false; (MAX_PIECE * MIN_PIECE) as usize];

            let mut num = 0;
            let mut rgn = 'A' as i8 + num;
            regions.push(rgn);
        
            // size_of[rgn] = 1;
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
                    rgn = 'A' as i8 + num;
                    regions[col] = rgn;
                    size_of[rgn as usize] = 1;
                    // WAIT A MINUTE this is as-written in the Python code, are
                    // they using addition as a non-commutative operator???
                    adjacent[(rgn+w) as usize] = true;
                    adjacent[(w+rgn) as usize] = true;
                }

                wall_bits >>= 1;
            }

            if region_too_big {
                continue;
            }

            let mut state = PolyominoGridEdge::new(
                Some(PolyominoGridEdgeStamp {
                    col: 0,
                    regions, size_of, adjacent
                })
            );

            // NOTE: might be worth it to fix this line...
            // assert![state_to_ways.contains_key(&state) == false];
            
            state_to_ways.insert(state.stamp(), ways);
        }
        todo!()
    };
}
