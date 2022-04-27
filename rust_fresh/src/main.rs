#![feature(is_sorted)]

type GridLen = u8;
type GridVol = u16;

type GridSliceStatePieceID = u8;
type GridSliceStateBound = u8;

macro_rules! dbgwrap {
    ($($args:expr),*) => { println!($($args),*) }
}

//macro_rules! dbgwrap {
//    ($($args:expr),*) => {}
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
                "{}",
                if self.id_to_len[self.pos_to_id[i] as usize] == self.pos_to_id.len() as u8 {
                    // NOTE: i'm really leaning on everything being a square
                    "_____"
                } else {
                    "     "
                }
            );
        }
        write!(f, "\n");
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
            );
        }
        write!(f, "\n");
        Ok(())
    }
}

impl GridSliceState {
    fn new(
        len: GridLen,
        cur_id_to_len_byte: &Vec<GridLen>,
        cur_pos_to_id_byte: &Vec<GridSliceStatePieceID>,
    ) -> Result<Self, ()> {
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
        } else if cur_pos_to_id_byte
            .iter()
            .all(|x| cur_id_to_len_byte[*x as usize] == len)
            && *cur_pos_to_id_byte != vec![2; len as usize]
        {
            // verify that squares are symmetric
            // vec![0; len] exists so we can choose one arbitrarily
            dbgwrap!("failed square symmetry test");
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
            dbgwrap!("failed increasing len test");
            Err(())
        } else if !cur_pos_to_id_byte
            .iter()
            .all(|x| cur_id_to_len_byte[*x as usize] > 0)
        {
            // every position must have a positive lenght piece
            dbgwrap!("failed every postion must have a positive length piece test");
            Err(())
        } else {
            Ok(GridSliceState {
                id_to_len: cur_id_to_len_byte.clone(),
                pos_to_id: cur_pos_to_id_byte.clone(),
            })
        }
    }

    fn get_len_by_id(&self, id: GridSliceStatePieceID) -> GridLen {
        self.id_to_len[id as usize]
    }

    fn get_id_by_pos(&self, pos: GridLen) -> GridSliceStatePieceID {
        self.pos_to_id[pos as usize]
    }
}

#[derive(Debug)]
struct GridSliceStateRelation {
    pub func: (GridSliceState, GridSliceState),
    pub coef: u64,
    pub base: (u64, u64),
}

impl GridSliceStateRelation {
    fn new(len: GridLen, x: &GridSliceState, y: &GridSliceState) -> Result<Self, ()> {
        if {
            // we need an intermediate value, let's call it surplus, which is
            // the difference between the length of the piece and the number of
            // positions. we can generate a recurrence relation as follows
            //   - the volume of pieces at our level is the same as the surplus
            //     from above (i.e. the only increase in volume comes from the
            //     transformation)
            //   - another can be generated if there is symmetry
            //
            // some things to keep in mind
            //   - every piece whose length is not the width of the board must
            //     continue to grow
            //   - we can assume (but need to use) a standardized representation
            //     of a slice state

            let id_to_surplus = x.id_to_len.iter().enumerate().map(|(x_id, x_len)| {
                // note: need to make some comments on this thought process
                len - x_len + x.pos_to_id.iter().filter(|x_id_| x_id as u8 == **x_id_).count() as u8 - 1
            }).collect::<Vec<_>>();

            panic!("x: {:#?}, {}, id_to_surplus: {:#?}", x, x, id_to_surplus)
        } {
            Ok(GridSliceStateRelation {
                func: (x.clone(), y.clone()),
                coef: todo!(), // the only way this can be >1 is by using symmetry
                base: (todo!(), todo!()),
            })
        } else {
            Err(())
        }
    }
}

#[derive(Debug)]
struct Grid {
    pub len: GridLen,
    pub slice_state: Vec<GridSliceState>,
    pub slice_state_relation: Vec<GridSliceStateRelation>,
}

impl Grid {
    fn new_slice_state(len: GridLen) -> Vec<GridSliceState> {
        // generate all slice states individually
        // note: this is a very naive way of doing it, but should be updated by the time I need to
        // present

        let mut ret = Vec::new();

        let mut add_with_carry = |val: &mut Vec<GridLen>, min: u8, max: u8| {
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

    fn new_slice_state_relation(
        len: GridLen,
        slice_state: &Vec<GridSliceState>,
    ) -> Vec<GridSliceStateRelation> {
        slice_state
            .iter()
            .flat_map(move |x| {
                slice_state
                    .iter()
                    .flat_map(move |y| GridSliceStateRelation::new(len, x, y))
            })
            .collect::<Vec<_>>()
    }

    fn new(len: GridLen) -> Self {
        // note: size is the length/height of the square and the size of each partitioned area
        let slice_state = Self::new_slice_state(len);
        let slice_state_relation = Self::new_slice_state_relation(len, &slice_state);
        Self {
            len,
            slice_state,
            slice_state_relation,
        }
    }
}

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {
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
        fn test_explicit_trominoes() {
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

        #[test]
        fn test_general_trominoes_slice_state() {
            let answer = vec![
                GridSliceState::new(3, &vec![1, 1, 1], &vec![0, 1, 2]).unwrap(),
                GridSliceState::new(3, &vec![0, 1, 2], &vec![1, 2, 2]).unwrap(),
                GridSliceState::new(3, &vec![2, 2, 2], &vec![0, 1, 2]).unwrap(),
                GridSliceState::new(3, &vec![0, 0, 3], &vec![2, 2, 2]).unwrap(), // A, A' and A''
                GridSliceState::new(3, &vec![1, 2, 3], &vec![0, 1, 2]).unwrap(),
            ];
            let grid = Grid::new(3);

            answer.iter().for_each(|x| {
                if !answer.iter().any(|y| x == y) {
                    println!("cannot find\n{}\nin results", x);
                }
            });

            assert_eq![grid.slice_state.len(), answer.len()];
        }
    }
}
