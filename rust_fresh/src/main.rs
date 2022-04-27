#![feature(is_sorted)]

type GridLen = u8;
type GridVol = u16;

type GridSliceStatePieceID = u8;
type GridSliceStateBound = u8;

#[derive(Debug, Eq, PartialEq, Hash)]
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
            println!("failed every non-zero length id has a position test");
            return Err(());
        }

        // the total volume of all placed pieces must be divisible by the piece size
        if cur_id_to_len_byte
            .iter()
            .map(|x| *x as GridVol)
            .sum::<GridVol>()
            % (len as GridVol)
            != 0
        {
            println!("failed divisibility test");
            return Err(());
        }

        // verify that squares are symmetric
        if cur_pos_to_id_byte
            .iter()
            .all(|x| cur_id_to_len_byte[*x as usize] == len)
        {
            // todo: actually add one
            println!("failed squarey symmetry test");
            return Err(());
        }

        // the id must have fewer (or equal) positions than length
        if !cur_id_to_len_byte.iter().enumerate().all(|(id, len)| {
            (*len as usize)
                >= cur_pos_to_id_byte
                    .iter()
                    .filter(|x| **x == id as u8)
                    .count()
        }) {
            println!("failed more or equal positions than length");
            return Err(());
        }

        // ids must be increasing in number from left to right (reduce isomorphisms)
        if !cur_pos_to_id_byte.iter().is_sorted() {
            println!("failed increasing id test");
            return Err(());
        }

        // every position must have a positive lenght piece
        if !cur_pos_to_id_byte
            .iter()
            .all(|x| cur_id_to_len_byte[*x as usize] > 0)
        {
            println!("failed every postion must have a positive length piece test");
            return Err(());
        }

        Ok(GridSliceState {
            id_to_len: cur_id_to_len_byte.clone(),
            pos_to_id: cur_pos_to_id_byte.clone(),
        })
    }

    fn get_len_by_id(&self, id: GridSliceStatePieceID) -> GridLen {
        self.id_to_len[id as usize]
    }

    fn get_id_by_pos(&self, pos: GridLen) -> GridSliceStatePieceID {
        self.pos_to_id[pos as usize]
    }
}

#[derive(Debug)]
struct GridSliceStateRecurrence {
    pub func: (GridSliceState, GridSliceState),
    pub coef: u64,
    pub base: (u64, u64),
}

#[derive(Debug)]
struct Grid {
    pub len: GridLen,
    pub slice_state: Vec<GridSliceState>,
    pub slice_state_relation: Vec<GridSliceStateRecurrence>,
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
                println!(
                    "{} {:?} {:?}",
                    len, &cur_id_to_len_byte, &cur_pos_to_id_byte
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
    ) -> Vec<GridSliceStateRecurrence> {
        // construct relations between slice states (i.e. build each equation in the recurrence
        // relation)
        Vec::new()
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
            let grid = Grid::new(3);
            for i in 0..grid.slice_state.len() {
                println!("{}:\n{}", i, grid.slice_state[i]);
            }
            panic!("grid.slice_state.len: {}", grid.slice_state.len());
        }
    }
}
