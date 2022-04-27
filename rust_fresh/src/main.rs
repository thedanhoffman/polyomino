#![feature(is_sorted)]

type GridLen = u8;
type GridVol = u16;

type GridSliceStatePieceID = u8;

//macro_rules! dbgwrap {
//    ($($args:expr),*) => { println!($($args),*) }
//}

macro_rules! dbgwrap {
    ($($args:expr),*) => {};
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
struct GridSliceState {
    // note: symmetries are not resolved in the type, so
    // multiple copies must be explicitly stored as-needed
    pub id_to_len: Vec<GridLen>,
    pub pos_to_id: Vec<GridSliceStatePieceID>,
}

impl std::fmt::Display for GridSliceState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in 0..self.pos_to_id.len() {
            write!(
                f,
                "{}",
                if self.id_to_len[self.pos_to_id[i] as usize] == self.pos_to_id.len() as u8 {
                    // NOTE: i'm really leaning on everything being a square
                    "_____"
                } else {
                    "     "
                }
            )
            .unwrap();
        }
        write!(f, "\n").unwrap();
        for i in 0..self.pos_to_id.len() {
            write!(
                f,
                "{} {} {}",
                {
                    if i == 0 || self.pos_to_id[i - 1] != self.pos_to_id[i] {
                        "|".to_owned()
                    } else {
                        " ".to_owned()
                    }
                },
                self.id_to_len[self.pos_to_id[i] as usize].to_string(),
                {
                    if i == self.pos_to_id.len() - 1 || self.pos_to_id[i + 1] != self.pos_to_id[i] {
                        "|".to_owned()
                    } else {
                        " ".to_owned()
                    }
                }
            )
            .unwrap();
        }
        write!(f, "\n").unwrap();
        Ok(())
    }
}

impl GridSliceState {
    fn new(
        len: GridLen,
        cur_id_to_len_byte: &Vec<GridLen>,
        cur_pos_to_id_byte: &Vec<GridSliceStatePieceID>,
    ) -> Result<Self, ()> {
        // the paper black-boxed how it generates the trominoes/nonominoes and the complexity here
        // is enough for the 20 minutes, so I'll be going over the symmetries involved

        // verify that every non-zero length id has a position
        if !cur_id_to_len_byte
            .iter()
            .enumerate()
            .all(|(id, len)| *len == 0 || cur_pos_to_id_byte.iter().any(|x| *x == id as u8))
        {
            dbgwrap!("failed every non-zero length id has a position test");
            Err(())
        } else if cur_id_to_len_byte
            .iter()
            .map(|x| *x as GridVol)
            .sum::<GridVol>()
            % (len as GridVol)
            != 0
        {
            // the total volume of all placed pieces must be divisible by the piece size
            dbgwrap!("failed divisibility test");
            Err(())
        } else if !cur_id_to_len_byte.iter().enumerate().all(|(id, len)| {
            (*len as usize)
                >= cur_pos_to_id_byte
                    .iter()
                    .filter(|x| **x == id as u8)
                    .count()
        }) {
            // the id must have fewer (or equal) positions than length
            dbgwrap!("failed more or equal positions than length");
            Err(())
        } else if !cur_pos_to_id_byte.iter().is_sorted() {
            // verify that the pos is always increasing
            dbgwrap!("failed increasing pos test");
            Err(())
        } else if !cur_id_to_len_byte.iter().is_sorted() {
            // verify that the len is always increasing
            dbgwrap!("failed increasing len test");
            Err(())
        } else if !cur_pos_to_id_byte
            .iter()
            .all(|x| cur_id_to_len_byte[*x as usize] > 0)
        {
            dbgwrap!("failed every postion must have a positive length piece test");
            Err(())
        } else if cur_pos_to_id_byte // palindromic in piece length
            .iter()
            .map(|x| cur_id_to_len_byte[*x as usize])
            .eq(cur_pos_to_id_byte
                .iter()
                .rev()
                .map(|x| cur_id_to_len_byte[*x as usize]))
            && cur_pos_to_id_byte
                .iter()
                .rev()
                .enumerate()
                .scan(Vec::new(), |state: &mut Vec<(u8, u8)>, x| {
                    // we use scan re-map it, effectively. note that enumerate comes
                    // after rev, so it indexes the reverse

                    dbg![x];
                    dbg![&state];

                    let ret = if x.0 > 0 {
                        // if the underlying has decreased by one, increase ours by one
                        let last_idx = x.0 - 1;
                        let curr_idx = x.0;

                        // dbg![last_idx;
                        // dbg![curr_idx];

                        assert![cur_pos_to_id_byte[curr_idx] >= cur_pos_to_id_byte[last_idx]];

                        // if we have already mapped this value, repeat it
                        // otherwise, if it has increased, add it to the map

                        if let Some(new_id) = state.iter().find(|x_| x_.0 == *x.1) {
                            Some(new_id.1)
                        } else {
                            state.push((
                                cur_pos_to_id_byte[curr_idx],
                                state.iter().max().unwrap().1 + 1,
                            ));
                            Some(state.iter().max().unwrap().1 + 1)
                        }
                    } else {
                        state.push((cur_pos_to_id_byte[x.0], cur_pos_to_id_byte[x.0]));
                        Some(cur_pos_to_id_byte[x.0])
                    };
                    dbg![ret];

                    ret
                })
                .lt(cur_pos_to_id_byte.iter().map(|x| *x))
        {
            // this one is a bit funky, had to take about seven pages of notes
            //
            // we need to canonicalize the form somehow, and there are cases where
            // we have identical output wtih different pos_to_id mappings
            dbgwrap!("failed the funky test");
            Err(())
        } else {
            Ok(GridSliceState {
                id_to_len: cur_id_to_len_byte.clone(),
                pos_to_id: cur_pos_to_id_byte.clone(),
            })
        }
    }

    fn has_unique_flip(&self) -> bool {
        !self
            .pos_to_id
            .iter()
            .map(|x_id| self.id_to_len[*x_id as usize])
            .eq(self
                .pos_to_id
                .iter()
                .map(|x_id| self.id_to_len[*x_id as usize])
                .rev())
    }
}

#[derive(Debug)]
struct GridSliceRelation {
    pub _func: (GridSliceState, GridSliceState),
    pub _coef: u64,
    pub _base: (u64, u64),
}

impl GridSliceRelation {
    fn new(_len: GridLen, x: &GridSliceState, y: &GridSliceState) -> Result<Self, ()> {
        if {
            // we can just map the current x_piece_id to y_piece_id and verify that the sum of
            // positions made by the piece is equal to the length at the second stage
            x.pos_to_id
                .iter()
                .zip(y.pos_to_id.iter())
                .all(|(x_id, y_id)| {
                    let discont =
                        x.id_to_len[*x_id as usize] == 3 && y.id_to_len[*y_id as usize] == 1;
                    let strict_inc = x.id_to_len[*x_id as usize] < y.id_to_len[*y_id as usize];
                    let accounting = y.id_to_len[*y_id as usize] as i16
                        == x.id_to_len[*x_id as usize] as i16
                            + y.pos_to_id.iter().filter(|y_id_| *y_id_ == y_id).count() as i16;

                    dbgwrap!("attempting to combine\n{}{}", y, x);
                    dbgwrap!("discont: {}", discont);
                    dbgwrap!(
                        "strict_inc: {} ({} < {})",
                        strict_inc,
                        x.id_to_len[*x_id as usize],
                        y.id_to_len[*y_id as usize]
                    );
                    dbgwrap!(
                        "accounting: {} ({} == {} - {})",
                        accounting,
                        y.id_to_len[*y_id as usize],
                        x.id_to_len[*x_id as usize],
                        y.pos_to_id.iter().filter(|y_id_| *y_id_ == y_id).count() as u8
                    );

                    discont || (strict_inc && accounting)
                })
        } {
            Ok(GridSliceRelation {
                _func: (x.clone(), y.clone()),
                // note: i think we only need to consider one symmetry, because any action on the
                // other simultaneously will double-count the horizontal symmetry
                _coef: if x.has_unique_flip() { 2 } else { 1 },
                _base: (0, 0),
            })
        } else {
            Err(())
        }
    }
}

#[derive(Debug)]
struct Grid {
    pub _len: GridLen,
    pub slice_state: Vec<GridSliceState>,
    pub slice_relation: Vec<GridSliceRelation>,
}

impl Grid {
    fn new_slice_state(len: GridLen) -> Vec<GridSliceState> {
        // generate all slice states individually
        // note: this is a very naive way of doing it, but should be updated by the time I need to
        // present

        let mut ret = Vec::new();

        let add_with_carry = |val: &mut Vec<GridLen>, min: u8, max: u8| {
            let mut pos = 0;
            while pos < val.len() && val[pos] == max {
                val[pos] = min;
                pos += 1;
            }

            if pos < val.len() as usize {
                val[pos] += 1;
                true
            } else {
                false
            }
        };

        let mut cur_id_to_len_byte = vec![0; len as usize];
        let mut cur_pos_to_id_byte = vec![0; len as usize];

        while add_with_carry(&mut cur_id_to_len_byte, 0, len) {
            while add_with_carry(&mut cur_pos_to_id_byte, 0, len - 1) {
                dbgwrap!(
                    "{} {:?} {:?}",
                    len,
                    &cur_id_to_len_byte,
                    &cur_pos_to_id_byte
                );
                if let Ok(t) = GridSliceState::new(len, &cur_id_to_len_byte, &cur_pos_to_id_byte) {
                    ret.push(t)
                }
            }
        }

        ret
    }

    fn new_slice_relation(
        len: GridLen,
        slice_state: &Vec<GridSliceState>,
    ) -> Vec<GridSliceRelation> {
        slice_state
            .iter()
            .flat_map(move |x| {
                slice_state
                    .iter()
                    .flat_map(move |y| GridSliceRelation::new(len, x, y))
            })
            .collect::<Vec<_>>()
    }

    fn new(len: GridLen) -> Self {
        // note: size is the length/height of the square and the size of each partitioned area
        let slice_state = Self::new_slice_state(len);
        let slice_relation = Self::new_slice_relation(len, &slice_state);
        Self {
            _len: len,
            slice_state,
            slice_relation,
        }
    }
}

fn main() {
    let grid = Grid::new(3);

    println!("slice_state: {:#?}", &grid.slice_state);
    println!("slice_relation: {:#?}", &grid.slice_relation);
}

#[cfg(test)]
mod tests {
    use super::*;

    mod trominoes {
        use super::*;

        mod explicit {
            fn a(n: u64) -> u64 {
                if n == 0 {
                    1
                } else {
                    a(n - 1) + c(n - 1) + d(n - 1) + e(n - 1) + g(n - 1)
                }
            }

            fn b(n: u64) -> u64 {
                if n == 0 {
                    0
                } else {
                    c(n - 1)
                }
            }

            fn c(n: u64) -> u64 {
                if n == 0 {
                    0
                } else {
                    d(n - 1) + g(n - 1)
                }
            }

            fn d(n: u64) -> u64 {
                if n == 0 {
                    0
                } else {
                    b(n - 1) + g(n - 1)
                }
            }

            fn e(n: u64) -> u64 {
                if n == 0 {
                    0
                } else {
                    f(n - 1)
                }
            }

            fn f(n: u64) -> u64 {
                if n == 0 {
                    0
                } else {
                    a(n - 1)
                }
            }

            fn g(n: u64) -> u64 {
                if n == 0 {
                    0
                } else {
                    2 * a(n - 1)
                }
            }

            #[test]
            fn test_trominoes_explicit() {
                assert_eq![a(1), 1];
                assert_eq![a(2), 3];
                assert_eq![a(3), 10];
                assert_eq![a(4), 23];
                assert_eq![a(5), 62];
                assert_eq![a(6), 170];
                assert_eq![a(7), 441];
                assert_eq![a(8), 1173];
                assert_eq![a(9), 3127];
                assert_eq![a(10), 8266];
            }
        }

        mod general {
            use super::*;

            mod slice_state {}

            mod slice_relation {}

            #[test]
            fn test_trominoes_general_slice_state_funky() {
                let a = GridSliceState::new(3, &vec![0, 3, 3], &vec![1, 1, 2]).is_ok();
                let b = GridSliceState::new(3, &vec![0, 3, 3], &vec![1, 2, 2]).is_ok();

                dbg![(a, b)];

                assert![(a ^ b) && (a || b)];
            }

            #[test]
            fn test_trominoes_general_slice_state() {
                let answer = vec![
                    GridSliceState::new(3, &vec![1, 1, 1], &vec![0, 1, 2]).unwrap(),
                    GridSliceState::new(3, &vec![0, 1, 2], &vec![1, 2, 2]).unwrap(),
                    GridSliceState::new(3, &vec![2, 2, 2], &vec![0, 1, 2]).unwrap(),
                    GridSliceState::new(3, &vec![0, 0, 3], &vec![2, 2, 2]).unwrap(), // A, A' and A''
                    GridSliceState::new(3, &vec![0, 3, 3], &vec![1, 2, 2]).unwrap(),
                    GridSliceState::new(3, &vec![3, 3, 3], &vec![0, 1, 2]).unwrap(),
                    GridSliceState::new(3, &vec![1, 2, 3], &vec![0, 1, 2]).unwrap(),
                ];
                let grid = Grid::new(3);

                answer.iter().for_each(|x| {
                    if !answer.iter().any(|y| x == y) {
                        println!("cannot find\n{}\nin results", x);
                    }
                });

                grid.slice_state.iter().for_each(|x| println!("{:?}", x));

                assert_eq![grid.slice_state.len(), answer.len()];

                grid.slice_state.iter().for_each(|x| println!("{}", x));
            }

            #[test]
            fn test_trominoes_general_slice_relation() {
                let grid = Grid::new(3);

                grid.slice_relation.iter().enumerate().for_each(|(i, x)| {
                    println!(
                        "{}:\n{:?}\n{:?} with coef {} and base ({}, {})",
                        i, x._func.1, x._func.0, x._coef, x._base.0, x._base.1
                    );
                });

                // assert![false];
            }
        }
    }
}
