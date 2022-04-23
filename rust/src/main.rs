
struct PolyominoGridEdgeStamp {
    col: i32,
    regions: Vec<u8>,
    size_of: u128,
    adjacent: u128
}
struct PolyominoGridEdge {
    // from __init__
    pges: Option<PolyominoGridEdgeStamp>,

    // from set_space
    num_cols: usize,
    min_piece: i32,
    max_piece: i32,
    stamp_bits: i32,
    piece_bits: i32
}

impl PolyominoGridEdge {
    fn new(
        num_cols: usize, 
        min_piece: i32, 
        max_piece: i32, 
    ) -> Self {
        assert![min_piece >= 2];
        assert![max_piece >= min_piece];
        assert![num_cols >= 1 && num_cols <= 25];

        Self {
            num_cols,
            min_piece,
            max_piece,
            stamp_bits: if num_cols < 4 {
                2
            } else if num_cols < 8 {
                3
            } else if num_cols < 16 {
                4
            } else {
                5
            },
            piece_bits: if max_piece < 4 {
                2
            } else if max_piece < 8 {
                3
            } else if max_piece < 16 {
                4
            } else {
                5
            },
            pges: None
        }
    }

    fn from_stamp() -> Self {
        todo!()
    }
}

fn main() {
    const max_piece: i32 = 5;
    const min_piece: i32 = 5;
    const num_rows: usize = 5;
    const num_cols: usize = 5;
    const column_step: i32 = num_cols as i32;
    const cache_limit: i32 = 0;
    const backtrack: bool = false;
    const progress: bool = false;
    
    let mut pge = PolyominoGridEdge::new(
        num_cols,
        max_piece,
        min_piece
    );

    #[derive(Default)]
    struct PolyominoStateToExpansion {};

    let mut state_to_expansion: [PolyominoStateToExpansion; num_cols];
    
    let mut num_cached_states = 0;
    let mut expansion_cnt = 0;
    let mut expansion_len = 0;

    let mut initial_states = {
        let mut num_walls = num_cols - 1;
        let mut state_to_ways: Vec<u8> = Vec::new();

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
            let mut size_of = [0; (max_piece * min_piece) as usize];
            let mut adjacent = [false; (max_piece * min_piece) as usize];

            let mut num = 0;
            let mut rgn = 'A' as i8 + num;
            regions.push(rgn);
        
            // size_of[rgn] = 1;
            let mut region_too_big = false;
        
            for col in 1..num_cols {
                if wall_bits & 1 == 0 {
                    regions[col] = rgn;
                    size_of[rgn as usize] += 1;
                    if size_of[rgn as usize] > max_piece {
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
        }
        todo!()
    };
}
