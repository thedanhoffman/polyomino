#![feature(is_sorted)]

type GridLen = u8;
type GridVol = u16;

type GridSliceStatePieceID = u8;

macro_rules! dbgwrap {
    ($($args:expr),*) => { println!($($args),*) }
}

//macro_rules! dbgwrap {
//    ($($args:expr),*) => {};
//}

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
        Ok(())
    }
}

impl GridSliceState {
    fn pass_non_zero_len_has_pos(
        cur_id_to_len_byte: &Vec<GridLen>,
        cur_pos_to_id_byte: &Vec<GridSliceStatePieceID>,
    ) -> bool {
        cur_id_to_len_byte
            .iter()
            .enumerate()
            .all(|(id, len)| *len == 0 || cur_pos_to_id_byte.iter().any(|x| *x == id as u8))
    }

    fn pass_volume_divisible_by_piece_size(
        cur_id_to_len_byte: &Vec<GridLen>,
        cur_pos_to_id_byte: &Vec<GridSliceStatePieceID>,
    ) -> bool {
        cur_id_to_len_byte
            .iter()
            .enumerate()
            .all(|(id, len)| *len == 0 || cur_pos_to_id_byte.iter().any(|x| *x == id as u8))
    }

    fn pass_no_more_pos_than_len(
        cur_id_to_len_byte: &Vec<GridLen>,
        cur_pos_to_id_byte: &Vec<GridSliceStatePieceID>,
    ) -> bool {
        cur_id_to_len_byte.iter().enumerate().all(|(id, len)| {
            (*len as usize)
                >= cur_pos_to_id_byte
                    .iter()
                    .filter(|x| **x == id as u8)
                    .count()
        })
    }

    fn pass_id_len_always_inc(
        cur_id_to_len_byte: &Vec<GridLen>,
        _cur_pos_to_id_byte: &Vec<GridSliceStatePieceID>,
    ) -> bool {
        cur_id_to_len_byte.iter().is_sorted()
    }

    fn pass_every_pos_has_len(
        cur_id_to_len_byte: &Vec<GridLen>,
        cur_pos_to_id_byte: &Vec<GridSliceStatePieceID>,
    ) -> bool {
        cur_pos_to_id_byte
            .iter()
            .all(|x| cur_id_to_len_byte[*x as usize] > 0)
    }

    fn pass_not_palindromic(
        cur_id_to_len_byte: &Vec<GridLen>,
        cur_pos_to_id_byte: &Vec<GridSliceStatePieceID>,
    ) -> bool {
        !(cur_pos_to_id_byte // palindromic in piece length
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

                    if x.0 > 0 {
                        // if the underlying has decreased by one, increase ours by one
                        let last_idx = x.0 - 1;
                        let curr_idx = x.0;

                        // assert![cur_pos_to_id_byte[curr_idx] >= cur_pos_to_id_byte[last_idx]];

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
                        // take the lowest possible value for the given piece
                        // note this is obviously wrong and should be fixed very very soon
                        state.push((cur_pos_to_id_byte[x.0], cur_pos_to_id_byte[x.0]));
                        Some(cur_pos_to_id_byte[x.0])
                    }
                })
                .lt(cur_pos_to_id_byte.iter().map(|x| *x)))
    }

    fn new(
        len: GridLen,
        cur_id_to_len_byte: &Vec<GridLen>,
        cur_pos_to_id_byte: &Vec<GridSliceStatePieceID>,
    ) -> Result<Self, ()> {
        let checks = [
            (
                "non zero len has pos",
                Self::pass_non_zero_len_has_pos
                    as fn(&Vec<GridLen>, &Vec<GridSliceStatePieceID>) -> bool,
            ),
            (
                "volume divisible by piece size",
                Self::pass_volume_divisible_by_piece_size
                    as fn(&Vec<GridLen>, &Vec<GridSliceStatePieceID>) -> bool,
            ),
            (
                "no more pos than len",
                Self::pass_no_more_pos_than_len
                    as fn(&Vec<GridLen>, &Vec<GridSliceStatePieceID>) -> bool,
            ),
            (
                "id len always inc",
                Self::pass_id_len_always_inc
                    as fn(&Vec<GridLen>, &Vec<GridSliceStatePieceID>) -> bool,
            ),
            (
                "every pos has len",
                Self::pass_every_pos_has_len
                    as fn(&Vec<GridLen>, &Vec<GridSliceStatePieceID>) -> bool,
            ),
            (
                "not palindromic",
                Self::pass_not_palindromic
                    as fn(&Vec<GridLen>, &Vec<GridSliceStatePieceID>) -> bool,
            ),
        ];

        checks.iter().for_each(|x| {
            if !(x.1)(&cur_id_to_len_byte, &cur_pos_to_id_byte) {
                panic!("test {} failed for id_to_len: {:?}, pos_to_id: {:?}", x.0, cur_id_to_len_byte, cur_pos_to_id_byte)
            }
        });

        Ok(GridSliceState {
            id_to_len: cur_id_to_len_byte.clone(),
            pos_to_id: cur_pos_to_id_byte.clone(),
        })
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
            // boundaries cannot be interior (i.e. it must be a polygon), so
            // we verify that two distinct neighboring pieces cannot merge
            !x.pos_to_id
                .iter()
                .zip(y.pos_to_id.iter())
                .enumerate()
                .all(|(pos, (x_id, y_id))| {
                    let x_distinct_left = pos == 0 || x.pos_to_id[pos - 1] == *x_id;
                    let x_distinct_right =
                        pos == x.pos_to_id.len() - 1 || x.pos_to_id[pos + 1] == *x_id;
                    let x_distinct_top =
                        x.id_to_len[*x_id as usize] == 3 && y.id_to_len[*y_id as usize] == 1;

                    let x_distinct_top_left = pos == 0
                        || x.id_to_len[x.pos_to_id[pos - 1] as usize] == 3
                            && y.id_to_len[y.pos_to_id[pos - 1] as usize] == 1;

                    let x_distinct_top_right = pos == x.pos_to_id.len() - 1
                        || x.id_to_len[x.pos_to_id[pos + 1] as usize] == 3
                            && y.id_to_len[y.pos_to_id[pos + 1] as usize] == 1;

                    let y_distinct_left = pos == 0 || y.pos_to_id[pos - 1] == *y_id;
                    let y_distinct_right =
                        pos == y.pos_to_id.len() - 1 || y.pos_to_id[pos + 1] == *y_id;

                    // no interior borders (i.e. no splits and no merges)
                    let no_int_bord = x_distinct_left as u8
                        + x_distinct_top as u8
                        + x_distinct_top_left as u8
                        + y_distinct_left as u8
                        != 1;

                    dbgwrap![
                        "{:#?}",
                        (
                            x_distinct_left as u8,
                            x_distinct_top as u8,
                            x_distinct_top_left as u8,
                            y_distinct_left as u8
                        )
                    ];

                    let y_id_vol = y.id_to_len[*y_id as usize] as i16;
                    let y_id_pos_vol =
                        y.pos_to_id.iter().filter(|y_id_| *y_id_ == y_id).count() as i16;
                    let x_id_vol = x.id_to_len[*x_id as usize] as i16;

                    dbgwrap!["{:#?}", (y_id_vol, y_id_pos_vol, x_id_vol)];

                    let exact_inc = x_distinct_top || y_id_vol - y_id_pos_vol == x_id_vol;

                    no_int_bord && exact_inc
                })
        } {
            dbgwrap!("slice relation failed the interior border test");
            Err(())
        } else {
            Ok(GridSliceRelation {
                _func: (x.clone(), y.clone()),
                // note: i think we only need to consider one symmetry, because any action on the
                // other simultaneously will double-count the horizontal symmetry
                _coef: if x.has_unique_flip() { 2 } else { 1 },
                _base: (0, 0),
            })
        }
    }
}

impl std::fmt::Display for GridSliceRelation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}*{}", self._func.0, self._coef, self._func.1).unwrap();
        Ok(())
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

    match std::env::args().nth(1).unwrap().as_str() {
        "ss" => grid.slice_state.iter().for_each(|x| println!("{}", x)),
        "sr" => grid.slice_relation.iter().for_each(|x| println!("{}", x)),
        _ => unreachable!(),
    }
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

            mod slice_state {
                use super::*;

                fn get_reference() -> Vec<GridSliceState> {
                    vec![
                        GridSliceState::new(3, &vec![0, 0, 3], &vec![2, 2, 2]).unwrap(), // A
                        GridSliceState::new(3, &vec![0, 3, 3], &vec![1, 2, 2]).unwrap(), // A'
                        GridSliceState::new(3, &vec![3, 3, 3], &vec![0, 1, 2]).unwrap(), // A''
                        GridSliceState::new(3, &vec![1, 2, 3], &vec![0, 1, 2]).unwrap(), // B
                        GridSliceState::new(3, &vec![1, 2, 3], &vec![2, 0, 1]).unwrap(), // C
                        GridSliceState::new(3, &vec![1, 2, 3], &vec![1, 2, 0]).unwrap(), // D
                        GridSliceState::new(3, &vec![2, 2, 2], &vec![0, 1, 2]).unwrap(), // E
                        GridSliceState::new(3, &vec![1, 1, 1], &vec![0, 1, 2]).unwrap(), // F
                        GridSliceState::new(3, &vec![0, 1, 2], &vec![2, 2, 1]).unwrap(), // G
                    ]
                }

                #[test]
                fn test_trominoes_general_slice_state_reference() {
                    get_reference();
                }

                #[test]
                fn test_trominoes_general_slice_state() {
                    let answer = get_reference();
                    let grid = Grid::new(3);

                    answer.iter().for_each(|x| {
                        if !answer.iter().any(|y| x == y) {
                            println!("cannot find\n{}\nin results", x);
                        }
                    });

                    grid.slice_state.iter().for_each(|x| println!("{:?}", x));
                    assert_eq![grid.slice_state.len(), answer.len()];
                }
            }

            mod slice_relation {
                use super::*;

                #[test]
                fn test_trominoes_general_slice_relation() {
                    let grid = Grid::new(3);

                    grid.slice_relation.iter().enumerate().for_each(|(i, x)| {
                        println!(
                            "{}:\n{}\n{} with coef {} and base ({}, {})",
                            i, x._func.1, x._func.0, x._coef, x._base.0, x._base.1
                        );
                    });

                    assert![false];
                }
            }
        }
    }
}
