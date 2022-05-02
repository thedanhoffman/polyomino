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
        _cur_pos_to_id_byte: &Vec<GridSliceStatePieceID>,
    ) -> bool {
        cur_id_to_len_byte
            .iter()
            .map(|x| *x as usize)
            .sum::<usize>()
            % 3
            == 0
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

    fn pass_not_symmetric(
        cur_id_to_len_byte: &Vec<GridLen>,
        cur_pos_to_id_byte: &Vec<GridSliceStatePieceID>,
    ) -> bool {
        // piece ids are isomorphic to each other, so we can't do anything about the values
        // themselves, we can only do things with equality between them. we define a canonical
        // representation of a slice state by putting the longest contiguous run of equal ids first
        // and using the case of a singular contiguous run always being true (as this is guaranteed
        // to be identical becuase of the isomorphism symmetry)

        // ideally there would be a filter_scan in Rust but I don't think that exists, so we have
        // to use a for loop here (or something equally ugly)

        let run_vec = {
            let mut ret = Vec::new();
            let mut start_pos = 0;
            let mut start_val = cur_pos_to_id_byte[0];
            for i in cur_pos_to_id_byte.iter().enumerate() {
                let x_pos = i.0;
                let x_val = *i.1;

                if start_val != x_val {
                    ret.push(x_pos - start_pos);
                    start_pos = x_pos;
                    start_val = x_val;
                }
            }
            ret.push(cur_pos_to_id_byte.len() - start_pos);
            ret
        };

        // note the last condition here makes assumptions that don't generalize beyond the 3 case
        run_vec[0] > run_vec[run_vec.len() - 1]
            || run_vec.len() == 1
            || (run_vec.iter().all(|x| *x == 1)
                && cur_pos_to_id_byte[0] < cur_pos_to_id_byte[cur_pos_to_id_byte.len() - 1])
    }

    fn pass_not_isomorphic(
        cur_id_to_len_byte: &Vec<GridLen>,
        cur_pos_to_id_byte: &Vec<GridSliceStatePieceID>,
    ) -> bool {
        cur_pos_to_id_byte
            .iter()
            .scan(Vec::new(), |state: &mut Vec<(u8, u8)>, x| {
                Some(if let Some(map) = state.iter().find(|y| y.0 == *x) {
                    map.1
                } else {
                    // find the first ID in the new domain that
                    //   1. has not already been mapped
                    //   2. is of the same length as the source
                    // find the first ID (in ascending order) that has
                    //   1. not been already mapped (must preserve distinct)
                    //   2. the current length mapping the new mapped length (cu
                    //
                    state.push((
                        *x,
                        cur_id_to_len_byte
                            .iter()
                            .enumerate()
                            .find(|(y_id, y_len)| {
                                state.iter().all(|z| z.0 != *y_id as u8) // not already been mapped
                                    && **y_len == cur_id_to_len_byte[*x as usize]
                            })
                            .unwrap()
                            .0 as u8,
                    ));
                    state[state.len() - 1].1
                })
            })
            .eq(cur_pos_to_id_byte.iter().map(|x| *x))
    }

    fn pass_not_disjoint(
        cur_id_to_len_byte: &Vec<GridLen>,
        cur_pos_to_id_byte: &Vec<GridSliceStatePieceID>,
    ) -> bool {
        // maximum distance between any two IDs on the slice is
        // (n - 4) because the piece must bridge that gap on
        // the upper/lower slice

        cur_pos_to_id_byte
            .iter()
            .enumerate()
            .scan(Vec::new(), |state: &mut Vec<(usize, u8)>, (x_pos, x_id)| {
                Some(
                    if let Some((y_pos, _y_id)) =
                        state.iter_mut().find(|(_y_pos, y_id)| x_id == y_id)
                    {
                        let ret = *y_pos == x_pos - 1
                            || (*y_pos as i8) < (cur_id_to_len_byte.len() as i8 - 4);
                        *y_pos = x_pos;
                        ret
                    } else {
                        state.push((x_pos, *x_id));
                        true
                    },
                )
            })
            .all(|x| x)
    }

    fn new(
        _len: GridLen,
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
                "not symmetric",
                Self::pass_not_symmetric as fn(&Vec<GridLen>, &Vec<GridSliceStatePieceID>) -> bool,
            ),
            (
                "not isomorphic",
                Self::pass_not_isomorphic as fn(&Vec<GridLen>, &Vec<GridSliceStatePieceID>) -> bool,
            ),
            (
                "not disjoint",
                Self::pass_not_disjoint as fn(&Vec<GridLen>, &Vec<GridSliceStatePieceID>) -> bool,
            ),
        ];

        if let Some(fail) = checks
            .iter()
            .find(|x| !(x.1)(&cur_id_to_len_byte, &cur_pos_to_id_byte))
        {
            dbgwrap!(
                "test {} failed for id_to_len: {:?}, pos_to_id: {:?}",
                fail.0,
                cur_id_to_len_byte,
                cur_pos_to_id_byte
            );
            Err(())
        } else {
            Ok(GridSliceState {
                id_to_len: cur_id_to_len_byte.clone(),
                pos_to_id: cur_pos_to_id_byte.clone(),
            })
        }
    }

    fn non_canonical_equal(a: &GridSliceState, b: &GridSliceState) -> bool {
        // note the overloaded equality operator is for the *canonical form*
        println!("checking {} and {}", a, b);
        format!["{}", a] == format!["{}", b]
    }

    fn reverse(mut a: GridSliceState) -> GridSliceState {
        a.pos_to_id.reverse();
        a
    }

    fn has_non_canonical_unique_flip(&self) -> bool {
        format!["{}", Self::reverse(self.clone())] != format!["{}", self]
    }
}

#[derive(Debug, Eq, PartialEq)]
struct GridSliceRelation {
    pub _func: (GridSliceState, GridSliceState),
    pub _piece_map: Vec<u8>,
}

impl GridSliceRelation {
    // note both of these functions are broken because a height of three is not a sufficient
    // condition for an upper border (look at the case of (l) in the paper), so we need to be
    // smarter about how we define borders between pieces

    fn pass_no_int_bord(x: &GridSliceState, y: &GridSliceState, piece_map: &Vec<u8>) -> bool {
        // note this, again, is specific to the 3x3 case but can be generalized through some
        // recursive function. i don't like doing this but its probably best to have a correct
        // baseline to work from

        // an interior border can only exist if there is one edge for a given four-way
        // intersection. we iterate through all x_pos and check whether the upper-right
        // intersection has more than one edge. we have an edge case to count eges of the square as
        // borders for obvious reasons
        x.pos_to_id.iter().enumerate().all(|(x_pos, x_id)| {
            let cross_left = piece_map[x.pos_to_id[x_pos] as usize] != y.pos_to_id[x_pos];
            let cross_right = x_pos == x.pos_to_id.len() - 1
                || piece_map[x.pos_to_id[x_pos + 1] as usize] != y.pos_to_id[x_pos + 1];
            let cross_up =
                x_pos == x.pos_to_id.len() - 1 || y.pos_to_id[x_pos] != y.pos_to_id[x_pos + 1];
            let cross_down = x_pos == x.pos_to_id.len() - 1 || *x_id != x.pos_to_id[x_pos + 1];

            cross_left as u8 + cross_right as u8 + cross_up as u8 + cross_down as u8 != 1
        })
    }

    fn pass_inc_vol(x: &GridSliceState, y: &GridSliceState, piece_map: &Vec<u8>) -> bool {
        // the volume of a piece must increase by the number of times it occurs in the next slice.
        // we know a piece occurs in the next slice if the length is less than three and we map the
        // x piece id to the y piece id because the ids are only unique within a slice)

        piece_map.iter().enumerate().all(|a| {
            let x_id = a.0 as u8;
            let y_id = *a.1;

            if y_id != 3 {
                let y_pos_vol = y
                    .pos_to_id
                    .iter()
                    .filter(|y_id_cur| **y_id_cur == y_id)
                    .count() as u8;

                // the x piece id length plus the number of positions in the new slice is the
                // length of the y piece id
                x.id_to_len[x_id as usize] + y_pos_vol == y.id_to_len[y_id as usize]
            } else {
                // either the piece id is unused or it is full
                x.id_to_len[x_id as usize] == 3 || x.id_to_len[x_id as usize] == 0
            }
        }) && y.id_to_len.iter().enumerate().all(|a| {
            let y_id = a.0 as u8;
            let y_len = *a.1;

            if !piece_map.iter().any(|y_id_cur| *y_id_cur == y_id) {
                // if the current piece has not been mapped from the x id, the volume must be the
                // same as the positional volume
                let y_pos_vol = y
                    .pos_to_id
                    .iter()
                    .filter(|y_id_cur| **y_id_cur == y_id)
                    .count() as u8;

                y_pos_vol == y.id_to_len[y_id as usize]
            } else {
                true
            }
        })
    }

    fn pass_map_valid_domain(x: &GridSliceState, y: &GridSliceState, piece_map: &Vec<u8>) -> bool {
        assert![piece_map.len() == 3];

        x.id_to_len.iter().enumerate().all(|(x_id, x_len)| if *x_len == 0 || *x_len == 3 { piece_map[x_id] == 3 } else { true }) // if the length of a piece in x is zero or three, then it does not map (an x piece does not have to map)
                    && y.id_to_len.iter().enumerate().all(|(y_id, y_len)| if *y_len == 0 { piece_map.iter().filter(|y_id_cur| **y_id_cur == y_id as u8).count() == 0 } else { true }) // if the length of a piece in y is zero, then it does not map
    }

    fn pass_connected(x: &GridSliceState, y: &GridSliceState, piece_map: &Vec<u8>) -> bool {
        // verify that each x<->y mapping have a connecting between them
        // note: this might also not generalize beyond the 3 case

        piece_map.iter().enumerate().all(|a| {
            let x_id = a.0 as u8;
            let y_id = *a.1;

            // either the map doesn't apply or there exists a connection between the ids
            y_id == 3
                || (0..piece_map.len())
                    .any(|pos| x.pos_to_id[pos] == x_id && y.pos_to_id[pos] == y_id)
        })
    }

    fn new(
        _len: GridLen,
        x: &GridSliceState,
        y: &GridSliceState,
        piece_map: &Vec<u8>,
    ) -> Result<Self, ()> {
        // note it might be a good idea to make these fn casts into a type alias
        let checks = [
            (
                "no int bord",
                Self::pass_no_int_bord as fn(&GridSliceState, &GridSliceState, &Vec<u8>) -> bool,
            ),
            (
                "inc vol",
                Self::pass_inc_vol as fn(&GridSliceState, &GridSliceState, &Vec<u8>) -> bool,
            ),
            (
                "map valid domain",
                Self::pass_map_valid_domain
                    as fn(&GridSliceState, &GridSliceState, &Vec<u8>) -> bool,
            ),
            (
                "connected",
                Self::pass_connected as fn(&GridSliceState, &GridSliceState, &Vec<u8>) -> bool,
            ),
        ];
        if let Some(fail) = checks.iter().find(|a| !(a.1)(x, y, piece_map)) {
            println!(
                "test {} failed for {} -> {} ({:?} -> {:?}) with piece_map {:?}",
                fail.0, x, y, x, y, piece_map
            );
            Err(())
        } else {
            println!(
                "tests passed for {} -> {} ({:?} -> {:?}) with piece_map {:?}",
                x, y, x, y, piece_map
            );
            Ok(GridSliceRelation {
                _func: (x.clone(), y.clone()),
                _piece_map: piece_map.clone(),
            })
        }
    }
}

impl std::fmt::Display for GridSliceRelation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}", self._func.0, self._func.1).unwrap();
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
    fn add_with_carry(val: &mut Vec<GridLen>, min: u8, max: u8) -> bool {
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
    }

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
        assert![len == 3];

        slice_state
            .iter()
            .flat_map(move |x| {
                slice_state.iter().flat_map(move |y| {
                    // note i should do all add_with_carry uses like this
                    (0..4).flat_map(move |first| {
                        (0..4).flat_map(move |second| {
                            (0..4).flat_map(move |third| {
                                GridSliceRelation::new(len, x, y, &vec![first, second, third])
                            })
                        })
                    })
                })
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

    fn find_slice_by_render(&self, a: &'static str) -> GridSliceState {
        self.slice_state
            .iter()
            .find(|x| format!["{}", x] == a)
            .unwrap()
            .clone()
    }

    fn solve_base(&self) -> GridSliceState {
        self.find_slice_by_render("| 3    3    3 |")
    }

    fn solve_iter(
        &self,
        slice_state: GridSliceState,
        stack: &mut Vec<GridSliceState>,
        tiling: &mut Vec<Vec<GridSliceState>>,
    ) {
        // note we don't need to define a base case for the recurrence because it is inherently
        // periodic

        // note this is a bit wonky. we use | 3    3    3 | as the top of the square and work from
        // there, but we don't render this since it isn't a real tile (it only has the properties
        // of the edge of a cube because the tiling is inherently periodic)

        // note we actually generate len + 2 tilings but force the top and bottom to be a
        // horizontal piece (identical behavior to the edge of the square)

        stack.push(slice_state.clone());

        if stack.len() as u8 == self._len + 2 {
            if slice_state == self.solve_base() {
                tiling.push(stack.clone());
            }
        } else {
            self.slice_relation.iter().enumerate().for_each(|x| {
                if GridSliceState::non_canonical_equal(&x.1._func.1, &slice_state) {
                    self.solve_iter(x.1._func.0.clone(), stack, tiling);
                } else if GridSliceState::non_canonical_equal(
                    &GridSliceState::reverse(x.1._func.1.clone()),
                    &slice_state,
                ) {
                    todo!();
                    self.solve_iter(GridSliceState::reverse(x.1._func.0.clone()), stack, tiling);
                }
            });
        };

        stack.truncate(stack.len() - 1);
    }

    fn solve(&self) -> usize {
        let mut stack = Vec::new();
        let mut tiling = Vec::new();
        self.solve_iter(self.solve_base(), &mut stack, &mut tiling);

        println!("tilings");
        tiling.iter().for_each(|x| {
            x.iter().for_each(|y| println!("{}", y));
            println!("");
        });

        tiling.len()
    }
}

fn main() {
    let grid = Grid::new(3);

    match std::env::args().nth(1).unwrap().as_str() {
        "ss" => grid.slice_state.iter().for_each(|x| println!("{}", x)),
        "sr" => grid.slice_relation.iter().for_each(|x| println!("{}", x)),
        "s" => println!("grid.solve(): {}", grid.solve()),
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use itertools::Itertools;

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

        mod unit {
            use super::*;

            mod slice_state {
                use super::*;

                #[test]
                fn test_trominoes_unit_slice_state_not_isomorphic() {
                    assert_eq![
                        (0..3)
                            .permutations(3)
                            .map(|x| {
                                GridSliceState::pass_not_isomorphic(&vec![3, 3, 3], &x) as usize
                            })
                            .sum::<usize>(),
                        1
                    ];
                }
            }
        }

        mod general {
            use super::*;

            mod slice_state {
                use super::*;

                fn get_reference() -> Vec<GridSliceState> {
                    vec![
                        GridSliceState::new(3, &vec![0, 0, 3], &vec![2, 2, 2]).unwrap(), // A
                        GridSliceState::new(3, &vec![0, 3, 3], &vec![1, 1, 2]).unwrap(), // A'
                        GridSliceState::new(3, &vec![3, 3, 3], &vec![0, 1, 2]).unwrap(), // A''
                        GridSliceState::new(3, &vec![1, 2, 3], &vec![0, 1, 2]).unwrap(), // B
                        GridSliceState::new(3, &vec![1, 2, 3], &vec![1, 0, 2]).unwrap(), // C
                        GridSliceState::new(3, &vec![1, 2, 3], &vec![0, 2, 1]).unwrap(), // D
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

                    grid.slice_state
                        .iter()
                        .for_each(|x| println!("{:?}\t{}", x, x));

                    answer.iter().for_each(|x| {
                        if !grid.slice_state.iter().any(|y| x == y) {
                            panic!("cannot find {:?} {} in results", x, x);
                        }
                    });

                    grid.slice_state.iter().for_each(|x| {
                        if !answer.iter().any(|y| x == y) {
                            panic!("cannot find {:?} {} in answers", x, x);
                        }
                    });

                    assert_eq![grid.slice_state.len(), answer.len()];
                }
            }

            mod slice_relation {
                use super::*;

                /*
                                fn get_reference() -> Vec<GridSliceRelation> {
                                    let slice_state = Grid::new_slice_state(3);
                                    let find = |a: &'static str| -> &GridSliceState {
                                        slice_state.iter().find(|x| format!["{}", x] == a).unwrap()
                                    };

                                    [
                                        ("| 1 || 1 || 1 |", "| 2 || 2 || 2 |"),
                                        ("| 2 || 2 || 2 |", "| 3 || 3 || 3 |"),
                                        ("| 3    3    3 |", "| 1 || 1 || 1 |"),
                                        ("| 3    3    3 |", "| 2    2 || 1 |"),
                                        ("| 3    3    3 |", "| 3    3    3 |"),
                                        ("| 1 || 3 || 2 |", "| 2 || 1 || 3 |"),
                                        ("| 1 || 3 || 2 |", "| 3    3 || 3 |"),
                                        ("| 3    3 || 3 |", "| 1 || 1 || 1 |"),
                                        ("| 3    3 || 3 |", "| 2    2 || 1 |"),
                                        ("| 3    3 || 3 |", "| 3    3    3 |"),
                                        ("| 3 || 3 || 3 |", "| 1 || 1 || 1 |"),
                                        ("| 3 || 3 || 3 |", "| 2    2 || 1 |"),
                                        ("| 3 || 3 || 3 |", "| 3    3    3 |"),
                                    ]
                                    .iter()
                                    .map(|x| {
                                        let ret = GridSliceRelation::new(3, find(x.0), find(x.1), todo!()).unwrap();
                                        ret
                                    })
                                    .collect::<Vec<_>>()
                                }

                                #[test]
                                fn test_trominoes_general_slice_relation_reference() {
                                    get_reference();
                                }
                */
                #[test]
                fn test_trominoes_general_slice_relation() {
                    let grid = Grid::new(3);
                    // let answers = get_reference();

                    grid.slice_relation.iter().enumerate().for_each(|(i, x)| {
                        println!("{}: {} -> {} with piece_map {:?}", i, x._func.0, x._func.1, x._piece_map);
                    });

                    assert![false];

                    /*
                                        answers.iter().for_each(|x| {
                                            if !grid.slice_relation.iter().any(|y| x == y) {
                                                panic!("cannot find {:?} {} in slice_relation", x, x);
                                            }
                                        });

                                        grid.slice_relation.iter().for_each(|x| {
                                            if !answers.iter().any(|y| x == y) {
                                                panic!("cannot find {:?} {} in answers", x, x)
                                            }
                                        });
                    */
                }
            }

            /*
            mod solve {
                use super::*;

                #[test]
                fn test_trominoes_general_solve() {
                    let grid = Grid::new(3);
                    assert_eq![grid.solve(), 10];
                }
            }
            */
        }
    }
}
