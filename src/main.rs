#![feature(is_sorted)]
#![feature(test)]
#![allow(soft_unstable)]

extern crate test;
use paste::paste;

type GridLen = u8;
type GridSliceStatePieceID = u8;

// helper stuff
struct AddWithCarry<const LENGTH: usize> {
    pub min: u8,
    pub max: u8,
    pub val: [u8; LENGTH],
}

impl<const LENGTH: usize> AddWithCarry<LENGTH> {
    fn new(min: u8, max: u8) -> Self {
        Self {
            min,
            max,
            val: [min; LENGTH],
        }
    }
}

impl<const LENGTH: usize> Iterator for AddWithCarry<LENGTH> {
    type Item = AddWithCarry<LENGTH>;

    fn next(&mut self) -> Option<AddWithCarry<LENGTH>> {
        let mut pos = 0;
        while pos < LENGTH && self.val[pos] == self.max {
            self.val[pos] = self.min;
            pos += 1;
        }

        if pos < LENGTH as usize {
            self.val[pos] += 1;
            Some(AddWithCarry::<LENGTH> {
                min: self.min,
                max: self.max,
                val: self.val,
            })
        } else {
            None
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
struct GridSliceState<const LENGTH: usize> {
    // note: symmetries are not resolved in the type, so
    // multiple copies must be explicitly stored as-needed
    pub id_to_len: [GridLen; LENGTH],
    pub pos_to_id: [GridSliceStatePieceID; LENGTH],
}

impl<const LENGTH: usize> std::fmt::Display for GridSliceState<LENGTH> {
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

impl<const LENGTH: usize> GridSliceState<LENGTH> {
    fn pass_non_zero_len_has_pos(
        cur_id_to_len_byte: &[u8; LENGTH],
        cur_pos_to_id_byte: &[u8; LENGTH],
    ) -> bool {
        cur_id_to_len_byte
            .into_iter()
            .enumerate()
            .all(|(id, len)| *len == 0 || cur_pos_to_id_byte.into_iter().any(|x| *x == id as u8))
    }

    fn pass_volume_divisible_by_piece_size(cur_id_to_len_byte: &[u8; LENGTH]) -> bool {
        cur_id_to_len_byte.into_iter().fold(0, |acc, x| acc + *x) % LENGTH as u8 == 0
    }

    fn pass_no_more_pos_than_len(
        cur_id_to_len_byte: &[u8; LENGTH],
        cur_pos_to_id_byte: &[u8; LENGTH],
    ) -> bool {
        cur_id_to_len_byte.into_iter().enumerate().all(|(id, len)| {
            (*len as usize)
                >= cur_pos_to_id_byte
                    .into_iter()
                    .filter(|x| **x == id as u8)
                    .count()
        })
    }

    fn pass_id_len_always_inc(cur_id_to_len_byte: &[u8; LENGTH]) -> bool {
        cur_id_to_len_byte.into_iter().is_sorted()
    }

    fn pass_every_pos_has_len(
        cur_id_to_len_byte: &[u8; LENGTH],
        cur_pos_to_id_byte: &[u8; LENGTH],
    ) -> bool {
        cur_pos_to_id_byte
            .into_iter()
            .all(|x| cur_id_to_len_byte[*x as usize] > 0)
    }

    fn pass_not_symmetric(cur_pos_to_id_byte: &[u8; LENGTH]) -> bool {
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
            for i in cur_pos_to_id_byte.into_iter().enumerate() {
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

        run_vec.iter().lt(run_vec.iter().rev())
            || (run_vec.iter().eq(run_vec.iter().rev())
                && cur_pos_to_id_byte
                    .into_iter()
                    .le(cur_pos_to_id_byte.into_iter().rev()))
    }

    fn pass_not_isomorphic(
        cur_id_to_len_byte: &[u8; LENGTH],
        cur_pos_to_id_byte: &[u8; LENGTH],
    ) -> bool {
        cur_pos_to_id_byte
            .into_iter()
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
                            .into_iter()
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
            .eq(cur_pos_to_id_byte.into_iter().map(|x| *x))
    }

    fn pass_not_disjoint(
        cur_id_to_len_byte: &[u8; LENGTH],
        cur_pos_to_id_byte: &[u8; LENGTH],
    ) -> bool {
        // maximum distance between any two IDs on the slice is
        // (n - 4) because the piece must bridge that gap on
        // the upper/lower slice

        cur_pos_to_id_byte
            .into_iter()
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

    fn has_valid_id_to_len(cur_id_to_len_byte: &[u8; LENGTH]) -> bool {
        Self::pass_volume_divisible_by_piece_size(cur_id_to_len_byte)
            && Self::pass_id_len_always_inc(cur_id_to_len_byte)
    }

    fn has_valid_pos_to_id(cur_pos_to_id_byte: &[u8; LENGTH]) -> bool {
        Self::pass_not_symmetric(cur_pos_to_id_byte)
    }

    fn has_valid_pair(
        cur_id_to_len_byte: &[u8; LENGTH],
        cur_pos_to_id_byte: &[u8; LENGTH],
    ) -> bool {
        Self::pass_non_zero_len_has_pos(cur_id_to_len_byte, cur_pos_to_id_byte)
            && Self::pass_no_more_pos_than_len(cur_id_to_len_byte, cur_pos_to_id_byte)
            && Self::pass_every_pos_has_len(cur_id_to_len_byte, cur_pos_to_id_byte)
            && Self::pass_not_isomorphic(cur_id_to_len_byte, cur_pos_to_id_byte)
            && Self::pass_not_disjoint(cur_id_to_len_byte, cur_pos_to_id_byte)
    }

    fn new(
        cur_id_to_len_byte: &[u8; LENGTH],
        cur_pos_to_id_byte: &[u8; LENGTH],
    ) -> Result<Self, ()> {
        if Self::has_valid_id_to_len(cur_id_to_len_byte)
            && Self::has_valid_pos_to_id(cur_pos_to_id_byte)
            && Self::has_valid_pair(cur_id_to_len_byte, cur_pos_to_id_byte)
        {
            Ok(GridSliceState::<LENGTH> {
                id_to_len: *cur_id_to_len_byte,
                pos_to_id: *cur_pos_to_id_byte,
            })
        } else {
            Err(())
        }
    }

    fn non_canonical_equal_full(
        a_id_to_len: &[u8; LENGTH],
        a_pos_to_id: &[u8; LENGTH],
        b_id_to_len: &[u8; LENGTH],
        b_pos_to_id: &[u8; LENGTH],
    ) -> bool {
        // note the overloaded equality operator is for the *canonical form*
        a_id_to_len.into_iter().eq(b_id_to_len.into_iter())
            && a_pos_to_id
                .into_iter()
                .map(|x| a_id_to_len[*x as usize])
                .eq(b_pos_to_id.into_iter().map(|x| b_id_to_len[*x as usize]))
            && a_pos_to_id
                .into_iter()
                .zip(a_pos_to_id.into_iter().skip(1))
                .map(|(a, b)| a == b)
                .eq(b_pos_to_id
                    .into_iter()
                    .zip(b_pos_to_id.into_iter().skip(1))
                    .map(|(a, b)| a == b))
    }

    fn non_canonical_equal(a: &GridSliceState<LENGTH>, b: &GridSliceState<LENGTH>) -> bool {
        Self::non_canonical_equal_full(&a.id_to_len, &a.pos_to_id, &b.id_to_len, &b.pos_to_id)
    }

    fn reverse(mut a: GridSliceState<LENGTH>) -> GridSliceState<LENGTH> {
        a.pos_to_id.reverse();
        a
    }

    fn symmetric(&self) -> bool {
        Self::non_canonical_equal_full(&self.id_to_len, &self.pos_to_id, &self.id_to_len, &{
            let mut tmp = self.pos_to_id;
            tmp.reverse();
            tmp
        })
    }

    fn pos_to_id(&self, flip: bool, pos: usize) -> u8 {
        if !flip {
            self.pos_to_id[pos as usize]
        } else {
            self.pos_to_id[self.pos_to_id.len() - pos as usize - 1]
        }
    }

    fn pos_to_id_iter<'a>(&'a self, flip: bool) -> Box<dyn Iterator<Item = u8> + 'a> {
        if !flip {
            Box::new(self.pos_to_id.into_iter())
        } else {
            Box::new(self.pos_to_id.into_iter().rev())
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct GridSliceRelation<const LENGTH: usize> {
    pub _func: (GridSliceState<LENGTH>, GridSliceState<LENGTH>),
    pub _piece_map: [u8; LENGTH],
    pub flip: bool,
}

impl<const LENGTH: usize> GridSliceRelation<LENGTH> {
    // note both of these functions are broken because a height of three is not a sufficient
    // condition for an upper border (look at the case of (l) in the paper), so we need to be
    // smarter about how we define borders between pieces

    fn pass_no_int_bord(
        x: &GridSliceState<LENGTH>,
        y: &GridSliceState<LENGTH>,
        piece_map: &[u8; LENGTH],
        flip: bool,
    ) -> bool {
        // note this, again, is specific to the 3x3 case but can be generalized through some
        // recursive function. i don't like doing this but its probably best to have a correct
        // baseline to work from

        // an interior border can only exist if there is one edge for a given four-way
        // intersection. we iterate through all x_pos and check whether the upper-right
        // intersection has more than one edge. we have an edge case to count eges of the square as
        // borders for obvious reasons
        x.pos_to_id_iter(flip).enumerate().all(|(x_pos, x_id)| {
            let y_id = y.pos_to_id(false, x_pos);

            let cross_right_up_down = if x_pos < x.pos_to_id.len() - 1 {
                let y_pos_right_id = y.pos_to_id(false, x_pos + 1);
                let x_pos_right_id = x.pos_to_id(flip, x_pos + 1);

                (piece_map[x_pos_right_id as usize] != y_pos_right_id) as u8
                    + (y_id != y_pos_right_id) as u8
                    + (x_id != x_pos_right_id) as u8
            } else {
                3
            };
            let cross_left = piece_map[x_id as usize] != y_id;

            cross_left as u8 + cross_right_up_down as u8 != 1
        })
    }

    fn pass_inc_vol(
        x: &GridSliceState<LENGTH>,
        y: &GridSliceState<LENGTH>,
        piece_map: &[u8; LENGTH],
    ) -> bool {
        // the volume of a piece must increase by the number of times it occurs in the next slice.
        // we know a piece occurs in the next slice if the length is less than three and we map the
        // x piece id to the y piece id because the ids are only unique within a slice)

        piece_map.into_iter().enumerate().all(|a| {
            let x_id = a.0 as u8;
            let y_id = *a.1;

            if y_id != y.id_to_len.len() as u8 {
                let y_pos_vol = y
                    .pos_to_id
                    .into_iter()
                    .filter(|y_id_cur| *y_id_cur == y_id)
                    .count() as u8;

                // the x piece id length plus the number of positions in the new slice is the
                // length of the y piece id
                x.id_to_len[x_id as usize] + y_pos_vol == y.id_to_len[y_id as usize]
            } else {
                // either the piece id is unused or it is full
                x.id_to_len[x_id as usize] == x.id_to_len.len() as u8
                    || x.id_to_len[x_id as usize] == 0
            }
        }) && y.id_to_len.into_iter().enumerate().all(|a| {
            let y_id = a.0 as u8;

            if !piece_map.into_iter().any(|y_id_cur| *y_id_cur == y_id) {
                // if the current piece has not been mapped from the x id, the volume must be the
                // same as the positional volume
                let y_pos_vol = y
                    .pos_to_id
                    .into_iter()
                    .filter(|y_id_cur| *y_id_cur == y_id)
                    .count() as u8;

                y_pos_vol == y.id_to_len[y_id as usize]
            } else {
                true
            }
        })
    }

    fn pass_map_valid_domain(
        x: &GridSliceState<LENGTH>,
        y: &GridSliceState<LENGTH>,
        piece_map: &[u8; LENGTH],
    ) -> bool {
        x.id_to_len.into_iter().enumerate().all(|(x_id, x_len)| if x_len == 0 || x_len == x.id_to_len.len() as u8 { piece_map[x_id] == x.id_to_len.len() as u8 } else { true }) // if the length of a piece in x is zero or three, then it does not map (an x piece does not have to map)
                    && y.id_to_len.into_iter().enumerate().all(|(y_id, y_len)| if y_len == 0 { piece_map.into_iter().filter(|y_id_cur| **y_id_cur == y_id as u8).count() == 0 } else { true })
        // if the length of a piece in y is zero, then it does not map
    }

    fn pass_connected(
        x: &GridSliceState<LENGTH>,
        y: &GridSliceState<LENGTH>,
        piece_map: &[u8; LENGTH],
        flip: bool,
    ) -> bool {
        // verify that each x<->y mapping have a connecting between them
        //
        // note: this might also not generalize beyond the 3 case

        piece_map.into_iter().enumerate().all(|a| {
            let x_id = a.0 as u8;
            let y_id = *a.1;

            // either the map doesn't apply or there exists a connection between the ids
            y_id == piece_map.len() as u8
                || (0..piece_map.len())
                    .any(|pos| x.pos_to_id(flip, pos) == x_id && y.pos_to_id[pos] == y_id)
        })
    }

    fn pass_unique_flip(
        x: &GridSliceState<LENGTH>,
        y: &GridSliceState<LENGTH>,
        _piece_map: &[u8; LENGTH],
        flip: bool,
    ) -> bool {
        // we want to have a second parity possible for everything possible ("flip" is whether we
        // flip the first. since the slice relation checks for a flip of *both* at the time we
        // check, we force the flip to be one value if it wouldn't change)
        if x.symmetric() || y.symmetric() {
            !flip
        } else {
            true
        }
    }

    fn has_valid_x_y_piece_map(
        x: &GridSliceState<LENGTH>,
        y: &GridSliceState<LENGTH>,
        piece_map: &[u8; LENGTH],
    ) -> bool {
        Self::pass_inc_vol(x, y, piece_map) && Self::pass_map_valid_domain(x, y, piece_map)
    }

    fn has_valid_set(
        x: &GridSliceState<LENGTH>,
        y: &GridSliceState<LENGTH>,
        piece_map: &[u8; LENGTH],
        flip: bool,
    ) -> bool {
        Self::pass_no_int_bord(x, y, piece_map, flip)
            && Self::pass_connected(x, y, piece_map, flip)
            && Self::pass_unique_flip(x, y, piece_map, flip)
    }

    fn new(
        _len: GridLen,
        x: &GridSliceState<LENGTH>,
        y: &GridSliceState<LENGTH>,
        piece_map: &[u8; LENGTH],
        flip: bool,
    ) -> Result<Self, ()> {
        if Self::has_valid_x_y_piece_map(x, y, piece_map)
            && Self::has_valid_set(x, y, piece_map, flip)
        {
            Ok(GridSliceRelation {
                _func: (x.clone(), y.clone()),
                _piece_map: piece_map.clone(),
                flip,
            })
        } else {
            Err(())
        }
    }
}

impl<const LENGTH: usize> std::fmt::Display for GridSliceRelation<LENGTH> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // note we can't easily re-cycle the display for an individual slice state because we want
        // to insert colors to make the piece mappings more obvious. these colors only have a
        // locally unique coloring (i.e. we only compute edges created between the two slices and a
        // slice between itself when coloring)

        let mut tiling = GridTiling::new();
        tiling.push((
            self._func.0.clone(),
            self._func.1.clone(),
            self._piece_map.clone(),
        ));
        write!(f, "{}", tiling.render_str()).unwrap();
        write!(
            f,
            "{} -> {} ({:?}, flip: {})",
            {
                let mut x = self._func.0.clone();
                if self.flip {
                    x.pos_to_id.reverse();
                }
                format!["{}", x]
            },
            self._func.1,
            self._piece_map,
            self.flip
        )
        .unwrap();
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct GridTiling<const LENGTH: usize> {
    // note we don't keep GridSliceRelation because we don't persist the flip bit and other things
    // like that
    pub slice_relation_stack: Vec<(GridSliceState<LENGTH>, GridSliceState<LENGTH>, [u8; LENGTH])>,
}

impl<const LENGTH: usize> GridTiling<LENGTH> {
    fn new() -> Self {
        Self {
            slice_relation_stack: Vec::new(),
        }
    }

    fn push(&mut self, a: (GridSliceState<LENGTH>, GridSliceState<LENGTH>, [u8; LENGTH])) {
        self.slice_relation_stack.push(a);
    }

    fn pop(&mut self) {
        self.slice_relation_stack
            .truncate(self.slice_relation_stack.len() - 1);
    }

    fn peek(&self) -> (GridSliceState<LENGTH>, GridSliceState<LENGTH>, [u8; LENGTH]) {
        self.slice_relation_stack[self.slice_relation_stack.len() - 1].clone()
    }

    fn len(&self) -> usize {
        self.slice_relation_stack.len()
    }

    fn render_graph_backtrack_slice_piece_id(
        &self,
        state: &mut Vec<(usize, u8, u8)>,
        slice_start: usize,
        piece_id: u8,
    ) -> u8 {
        // keep backtracking via piece_map until nothing maps back to the piece
        let cur_slice_pos = slice_start;

        // note that we map piece ids to global ids by a left/right position and a slice
        // relation, which implies one of two options. as a matter of convention we always use
        // the bottom-most id for the relation (i.e. we take the y of the first without a match
        // instead of the x of the last with a match)

        let (cur_slice_pos, cur_piece_id) = if cur_slice_pos == 0 {
            // the base case is the top of the first relation, so we define unique ids only with
            // left-right relations
            (slice_start, piece_id)
        } else {
            // all other relations assume the *bottom* of the relation is the position (so we
            // actually index 1..(n + 1) as the bottom of the 0..n relations)
            let mut cur_slice_pos = slice_start - 1;
            let mut cur_piece_id = piece_id;

            // while the current stack maps a previous id to the current id, go back
            while let Some(cur_map) = self.slice_relation_stack[cur_slice_pos]
                .2
                .into_iter()
                .enumerate()
                .find(|(_, cur_piece_y_id)| *cur_piece_y_id == cur_piece_id)
            {
                if cur_slice_pos == 0 {
                    break;
                }
                cur_slice_pos = cur_slice_pos - 1;
                cur_piece_id = cur_map.0 as u8;
            }
            (cur_slice_pos + 1, cur_piece_id)
        };

        if let Some(global_piece_map) = state
            .iter()
            .find(|x| x.0 == cur_slice_pos && x.1 == cur_piece_id)
        {
            global_piece_map.2
        } else {
            // note every piece at every slice above the current should be defined, so
            // it might be easier to re-write this to assert if that fails
            // this is a new piece, so add it
            let cur_global_id = state.len() as u8;
            state.push((cur_slice_pos, cur_piece_id, cur_global_id));
            cur_global_id
        }
    }

    fn render_graph(&self) -> (Vec<(u8, u8)>, Vec<(usize, u8, u8)>) {
        // note we go through the tiling slice by slice and do the following:
        //   - if this is a new piece, assign it the next available global id, otherwise fetch the
        //     piece id of the immediate previous piece (recursing until we reach the first piece
        //     and looking up the first piece in some map)
        //   - recurse up on parent id and add an edge in the global graph
        //   - recurse up on left and right and add an edge in the global graph
        //
        // the graph is represented as an adjacency list

        // note i need to insert a special case for the 0->1 mapping here

        let connect_slice = |ret: &mut Vec<(u8, u8)>,
                             state: &mut Vec<(usize, u8, u8)>,
                             cur_slice_pos: usize,
                             cur_slice: GridSliceState<LENGTH>,
                             cur_piece_pos: usize,
                             cur_piece_id: &u8| {
            let cur_id =
                self.render_graph_backtrack_slice_piece_id(state, cur_slice_pos, *cur_piece_id);

            // note the number is 3 because of the number of non-down ways that a polyomino can
            // border another, not because of an edge case with the 3 case

            ret.extend((0..3).flat_map(|x| {
                let pair = match x {
                    0 => {
                        // connect to left neighbor (if one exists)
                        if cur_piece_pos > 0 {
                            Some((
                                cur_id,
                                self.render_graph_backtrack_slice_piece_id(
                                    state,
                                    cur_slice_pos,
                                    cur_slice.pos_to_id[cur_piece_pos - 1],
                                ),
                            ))
                        } else {
                            None
                        }
                    }
                    1 => {
                        // connect to right neighbor (if one exists)
                        if cur_piece_pos < LENGTH - 1 {
                            Some((
                                cur_id,
                                self.render_graph_backtrack_slice_piece_id(
                                    state,
                                    cur_slice_pos,
                                    cur_slice.pos_to_id[cur_piece_pos + 1],
                                ),
                            ))
                        } else {
                            None
                        }
                    }
                    2 => {
                        // connect to top neighbor (if one exists)
                        if cur_slice_pos > 0 {
                            Some((
                                cur_id,
                                self.render_graph_backtrack_slice_piece_id(
                                    state,
                                    cur_slice_pos - 1,
                                    self.slice_relation_stack[cur_slice_pos - 1].0.pos_to_id
                                        [cur_piece_pos],
                                ),
                            ))
                        } else {
                            None
                        }
                    }
                    _ => unreachable!(),
                };

                if let Some(edge) = pair {
                    if edge.0 > edge.1 {
                        Some((edge.0, edge.1))
                    } else if edge.0 < edge.1 {
                        Some((edge.1, edge.0))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }));
        };

        let mut ret = Vec::new();
        let mut state = Vec::new();

        self.slice_relation_stack[0]
            .0
            .pos_to_id
            .iter()
            .enumerate()
            .for_each(|(cur_piece_pos, cur_piece_id)| {
                connect_slice(
                    &mut ret,
                    &mut state,
                    0,
                    self.slice_relation_stack[0].0.clone(),
                    cur_piece_pos,
                    cur_piece_id,
                );
            });

        for (cur_slice_pos, cur_slice) in self.slice_relation_stack.iter().enumerate() {
            for (cur_piece_pos, cur_piece_id) in cur_slice.1.pos_to_id.iter().enumerate() {
                connect_slice(
                    &mut ret,
                    &mut state,
                    cur_slice_pos + 1,
                    cur_slice.1.clone(),
                    cur_piece_pos,
                    cur_piece_id,
                );
            }
        }

        ret.sort();
        ret.dedup_by(|a, b| (a.0 == b.0 && a.1 == b.1));

        (ret, state)
    }

    fn render_color(&self, graph: &Vec<(u8, u8)>) -> [u8; 1024] {
        // note graph coloring is an interesting problem but its outside the scope here. we have
        // four as an upper bound for the number of colors (because of the four-color theorem, but
        // it is easier to show in the square-grid case), so we brute-force the 1, 2, 3 and 4
        // colorings and return the first

        // note for the nxn case, the number of n partitions is obviously n. however, because we
        // can render slice states, and not all valid slice states are possible in a tiling of a
        // square, this code needs to generalize to an arbitrary number of global piece ids, so we
        // can't use a static length vector here. we brute force the coloring in a similar way
        // (AddWithCarry) but we set the length to be the number of global ids total (the number of
        // colors, the base, is still 4 because of the four-color theorem, but the native
        // representation is a jump-table/function, so we need to scale with the size of the
        // domain). 1024 is a reasonably large number and i should find a better algorithm anyways...

        AddWithCarry::<1024>::new(0, LENGTH as u8)
            .find(|color| {
                // verify that no two colors are connected by an edge
                graph
                    .iter()
                    .all(|edge| (color.val)[edge.0 as usize] != (color.val)[edge.1 as usize])
            })
            .unwrap()
            .val
    }

    fn render_map(&self) -> Vec<Vec<(u8, u8)>> {
        let mut graph = self.render_graph();
        let color = self.render_color(&graph.0);

        let mut map_line =
            |cur_slice_relation_pos: usize, cur_slice: GridSliceState<LENGTH>| -> Vec<(u8, u8)> {
                cur_slice
                    .pos_to_id
                    .iter()
                    .map(|cur_piece_id| {
                        let global_id = self.render_graph_backtrack_slice_piece_id(
                            &mut graph.1,
                            cur_slice_relation_pos,
                            *cur_piece_id,
                        );
                        (global_id, color[global_id as usize])
                    })
                    .collect::<Vec<_>>()
            };

        let mut ret = Vec::new();
        ret.push(map_line(0, self.slice_relation_stack[0].0.clone()));
        self.slice_relation_stack.iter().enumerate().for_each(
            |(cur_slice_relation_pos, cur_slice_relation)| {
                ret.push(map_line(
                    cur_slice_relation_pos + 1,
                    cur_slice_relation.1.clone(),
                ));
            },
        );

        ret
    }

    fn render_str(&self) -> String {
        let map = self.render_map();

        map.iter()
            .map(|line| {
                line.iter()
                    .map(|val| {
                        format![
                            "\x1b[3{}m{}\x1b[39;49m",
                            match val.1 {
                                0 => 4,
                                1 => 1,
                                2 => 5,
                                3 => 2,
                                _ => unreachable!(),
                            },
                            if val.0 < 10 {
                                val.0.to_string().chars().nth(0).unwrap()
                            } else if val.0 < 36 {
                                std::char::from_u32(val.0 as u32 - 10 + 65).unwrap()
                            } else {
                                panic!("there isn't a good way to render this many pieces yet")
                            }
                        ]
                    })
                    .fold(String::new(), |a, b| format!["{}{}", a, b])
            })
            .fold(String::new(), |a, b| format!["{}{}\n", a, b])
    }

    fn render(&self) {
        println!("{}", self.render_str());
    }
}

#[derive(Debug)]
struct Grid<const LENGTH: usize> {
    pub slice_state: Vec<GridSliceState<LENGTH>>,
    pub slice_relation: Vec<GridSliceRelation<LENGTH>>,
}

impl<const LENGTH: usize> Grid<LENGTH> {
    fn new_slice_state() -> Vec<GridSliceState<LENGTH>> {
        // generate all slice states individually
        // note: this is a very naive way of doing it, but should be updated by the time I need to
        // present

        AddWithCarry::<LENGTH>::new(0, LENGTH as u8)
            .flat_map(move |cur_id_to_len_byte| {
                if GridSliceState::has_valid_id_to_len(&cur_id_to_len_byte.val) {
                    Some(AddWithCarry::<LENGTH>::new(0, LENGTH as u8 - 1).flat_map(
                        move |cur_pos_to_id_byte| {
                            if GridSliceState::has_valid_pos_to_id(&cur_pos_to_id_byte.val)
                                && GridSliceState::has_valid_pair(
                                    &cur_id_to_len_byte.val,
                                    &cur_pos_to_id_byte.val,
                                )
                            {
                                Some(GridSliceState {
                                    id_to_len: cur_id_to_len_byte.val,
                                    pos_to_id: cur_pos_to_id_byte.val,
                                })
                            } else {
                                None
                            }
                        },
                    ))
                } else {
                    None
                }
            })
            .flatten()
            .collect::<Vec<GridSliceState<LENGTH>>>()
    }

    fn new_slice_relation(
        slice_state: &Vec<GridSliceState<LENGTH>>,
    ) -> Vec<GridSliceRelation<LENGTH>> {
        slice_state
            .iter()
            .flat_map(move |x| {
                slice_state.iter().flat_map(move |y| {
                    // note i should do all add_with_carry uses like this
                    AddWithCarry::<LENGTH>::new(0, LENGTH as u8).flat_map(move |piece_map| {
                        if GridSliceRelation::has_valid_x_y_piece_map(x, y, &piece_map.val) {
                            Some((0..2).flat_map(move |flip| {
                                if GridSliceRelation::has_valid_set(x, y, &piece_map.val, flip != 0)
                                {
                                    Some(GridSliceRelation {
                                        _func: (x.clone(), y.clone()),
                                        _piece_map: piece_map.val.clone(),
                                        flip: flip != 0,
                                    })
                                } else {
                                    None
                                }
                            }))
                        } else {
                            None
                        }
                    })
                })
            })
            .flatten()
            .collect::<Vec<_>>()
    }

    fn new() -> Self {
        // note: size is the length/height of the square and the size of each partitioned area
        let slice_state = Self::new_slice_state();
        let slice_relation = Self::new_slice_relation(&slice_state);
        Self {
            slice_state,
            slice_relation,
        }
    }

    fn solve_base(&self) -> (GridSliceState<LENGTH>, GridSliceState<LENGTH>, [u8; LENGTH]) {
        let canonical = self
            .slice_state
            .iter()
            .find(|x| x.id_to_len.iter().filter(|x| **x == 0).count() == x.id_to_len.len() - 1)
            .unwrap()
            .clone();

        let canonical_relation = self
            .slice_relation
            .iter()
            .find(|x| x._func.0 == canonical && x._func.1 == canonical)
            .unwrap()
            .clone();

        (
            canonical_relation._func.0,
            canonical_relation._func.1,
            canonical_relation._piece_map,
        )
    }

    fn solve_iter(
        &self,
        stack: &mut GridTiling<LENGTH>,
        tiling: &mut Vec<GridTiling<LENGTH>>,
        depth: usize,
    ) {
        if stack.len() == depth + 3 {
            let prev = stack.peek();
            let base = self.solve_base();

            if GridSliceState::non_canonical_equal(&prev.0, &base.0)
                && GridSliceState::non_canonical_equal(&prev.1, &base.1)
            {
                tiling.push(stack.clone());
            }
        } else {
            let prev = stack.peek().1;

            self.slice_relation
                .iter()
                // we generate the valid flips (sets the parity, effectively)
                .map(|x| {
                    (
                        if x.flip {
                            GridSliceState::reverse(x._func.0.clone())
                        } else {
                            x._func.0.clone()
                        },
                        x._func.1.clone(),
                        x._piece_map.clone(),
                    )
                })
                // generate both the forward and backwards configuration
                .flat_map(|x| {
                    (0..2).flat_map(move |a| {
                        if a == 0 {
                            Some((x.0.clone(), x.1.clone(), x.2.clone()))
                        } else if !x.0.symmetric() || !x.1.symmetric() {
                            Some((
                                GridSliceState::reverse(x.0.clone()),
                                GridSliceState::reverse(x.1.clone()),
                                x.2.clone(),
                            ))
                        } else {
                            None
                        }
                    })
                })
                .filter(|x| GridSliceState::non_canonical_equal(&x.0, &prev))
                .for_each(|x| {
                    stack.push(x);
                    self.solve_iter(stack, tiling, depth);
                    stack.pop();
                });
        }
    }

    fn solve(&self, depth: usize) -> Vec<GridTiling<LENGTH>> {
        let mut stack = GridTiling::new();
        let mut tiling = Vec::new();

        stack.push(self.solve_base());
        self.solve_iter(&mut stack, &mut tiling, depth);

        println!("tilings (length = {})", tiling.len());
        tiling.iter().enumerate().for_each(|x| {
            println!("TILING {}", x.0);
            x.1.render();
        });

        tiling
    }
}

fn main() {
    match std::env::args().nth(1).unwrap().as_str() {
        "render" => {
            let depth = std::env::args().nth(3).unwrap().parse().unwrap();

            match std::env::args().nth(2).unwrap().parse::<u8>().unwrap() {
                2 => {
                    Grid::<2>::new().solve(depth);
                }
                3 => {
                    Grid::<3>::new().solve(depth);
                }
                4 => {
                    Grid::<4>::new().solve(depth);
                }
                5 => {
                    Grid::<5>::new().solve(depth);
                }
                6 => {
                    Grid::<6>::new().solve(depth);
                }
                _ => unreachable!(),
            };
        }
        "scratch" => {
            let grid = Grid::<4>::new().solve(4);

            println!("\n\n");
            println!("THE ONLY ONES I CARE ABOUT");
            [131, 96, 71, 69, 68, 65, 64, 63, 58]
                .into_iter()
                .for_each(|x| grid[x].render());
        }
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

        mod slice_state {
            use super::*;

            fn get_reference() -> Vec<GridSliceState<3>> {
                // note: the paper says seven slice states because it has A = A' = A''
                vec![
                    GridSliceState::new(&[0, 0, 3], &[2, 2, 2]).unwrap(), // A
                    GridSliceState::new(&[0, 3, 3], &[1, 2, 2]).unwrap(), // A'
                    GridSliceState::new(&[3, 3, 3], &[0, 1, 2]).unwrap(), // A''
                    GridSliceState::new(&[1, 2, 3], &[0, 1, 2]).unwrap(), // B
                    GridSliceState::new(&[1, 2, 3], &[1, 0, 2]).unwrap(), // C
                    GridSliceState::new(&[1, 2, 3], &[0, 2, 1]).unwrap(), // D
                    GridSliceState::new(&[2, 2, 2], &[0, 1, 2]).unwrap(), // E
                    GridSliceState::new(&[1, 1, 1], &[0, 1, 2]).unwrap(), // F
                    GridSliceState::new(&[0, 1, 2], &[1, 2, 2]).unwrap(), // G
                ]
            }

            #[test]
            fn test_trominoes_slice_state_reference() {
                get_reference();
            }

            #[test]
            fn test_trominoes_slice_state() {
                let answer = get_reference();
                let grid = Grid::<3>::new();

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

            fn get_reference() -> Vec<GridSliceRelation<3>> {
                let slice_state = Grid::<3>::new_slice_state();
                let find = |a: &'static str| -> &GridSliceState<3> {
                    slice_state.iter().find(|x| format!["{}", x] == a).unwrap()
                };

                [
                    ("| 1 || 1 || 1 |", "| 2 || 2 || 2 |", [0, 1, 2], false),
                    ("| 1 || 2    2 |", "| 1 || 3 || 2 |", [3, 1, 2], true),
                    ("| 1 || 2    2 |", "| 2 || 1 || 3 |", [3, 1, 2], false),
                    ("| 1 || 2    2 |", "| 3 || 3    3 |", [3, 2, 1], true),
                    ("| 2 || 2 || 2 |", "| 3 || 3 || 3 |", [0, 1, 2], false),
                    ("| 3    3    3 |", "| 1 || 1 || 1 |", [3, 3, 3], false),
                    ("| 3    3    3 |", "| 1 || 2    2 |", [3, 3, 3], false),
                    ("| 3    3    3 |", "| 3    3    3 |", [3, 3, 3], false),
                    ("| 1 || 3 || 2 |", "| 2 || 1 || 3 |", [1, 2, 3], false),
                    ("| 1 || 3 || 2 |", "| 3 || 3    3 |", [2, 1, 3], true),
                    ("| 2 || 1 || 3 |", "| 1 || 2 || 3 |", [1, 2, 3], true),
                    ("| 2 || 1 || 3 |", "| 3 || 3    3 |", [2, 1, 3], false),
                    ("| 1 || 2 || 3 |", "| 1 || 3 || 2 |", [1, 2, 3], true),
                    ("| 3 || 3    3 |", "| 1 || 1 || 1 |", [3, 3, 3], false),
                    ("| 3 || 3    3 |", "| 1 || 2    2 |", [3, 3, 3], false),
                    ("| 3 || 3    3 |", "| 1 || 2    2 |", [3, 3, 3], true),
                    ("| 3 || 3    3 |", "| 3    3    3 |", [3, 3, 3], false),
                    ("| 3 || 3 || 3 |", "| 1 || 1 || 1 |", [3, 3, 3], false),
                    ("| 3 || 3 || 3 |", "| 1 || 2    2 |", [3, 3, 3], false),
                    ("| 3 || 3 || 3 |", "| 3    3    3 |", [3, 3, 3], false),
                ]
                .iter()
                .map(|x| {
                    let ret = GridSliceRelation::new(3, find(x.0), find(x.1), &x.2, x.3).unwrap();
                    ret
                })
                .collect::<Vec<_>>()
            }

            #[test]
            fn test_trominoes_slice_relation_reference() {
                get_reference();
            }
            #[test]
            fn test_trominoes_slice_relation() {
                let grid = Grid::<3>::new();

                grid.slice_relation.iter().enumerate().for_each(|(i, x)| {
                    println!(
                        "{}: {} -> {} with piece_map {:?} and flip {}",
                        i, x._func.0, x._func.1, x._piece_map, x.flip
                    );
                });

                let answers = get_reference();
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
            }
        }
        mod solve {
            use super::*;

            #[test]
            fn test_trominoes_solve() {
                let grid = Grid::<3>::new();
                assert_eq![grid.solve(3).len(), 10];
            }
        }
    }

    mod tetrominoes {
        use super::*;

        #[test]
        fn test_tetrominoes_solve() {
            let grid = Grid::<4>::new();
            assert_eq![grid.solve(4).len(), 147]; // note im unsure whether this number is correct
        }
    }

    mod general {
        use super::*;

        fn test_general_connected<const LENGTH: usize>() {
            // verify that each ID creates a singular connected region (this currently fails
            // because of the render code,
            let grid = Grid::<LENGTH>::new();

            assert![grid.solve(LENGTH).iter().all(|tiling| {
                let render_str = tiling.render_str();
                let render_map = tiling.render_map();
                println!("render_str: {:?}", &render_str);
                println!("render_map: {:?}", &render_map);

                // note we check whether a configuration is connected by calculating a set of
                // "feasible regions" by recognizing that an area cannot exist beyond the exterior
                // of any given edge (unless, of course, another exterior cancels it with an
                // interior on the other side). the following is a linear time algorithm to verify
                // that, given an area (in this case, LENGTH) and a set of pieces, that all pieces
                // are connected together
                //
                // note "feasible area" is a bit of a misnomer. although this phrase is borrowed
                // from linear programming, we don't construct it as the intersection/union of
                // half planes, but instead apply it specifically to the region orthogonal to the
                // edge extending from the exterior inwards. it should be obvious why half planes
                // will not work

                // note we do this because rust's 'move' with a closure moves *all* variables
                // referenced. this layer of indirection forces the move to be moving a reference,
                // which is de-facto a pass-by-reference
                let render_map = &render_map;

                let feas_reg = render_map
                    .iter()
                    .enumerate()
                    .map(move |(y, row)| {
                        row.iter().enumerate().map(move |(x, val)| {
                            (0..4).filter_map(move |dir| match dir {
                                0 => {
                                    if y < render_map.len() - 1 && render_map[y + 1][x].0 != val.0 {
                                        Some((x, y, 'U'))
                                    } else {
                                        None
                                    }
                                }
                                1 => {
                                    if y > 0 && render_map[y - 1][x].0 != val.0 {
                                        Some((x, y, 'D'))
                                    } else {
                                        None
                                    }
                                }
                                2 => {
                                    if x < render_map[y].len() - 1
                                        && render_map[y][x + 1].0 != val.0
                                    {
                                        Some((x, y, 'R'))
                                    } else {
                                        None
                                    }
                                }
                                3 => {
                                    if x > 0 && render_map[y][x - 1].0 != val.0 {
                                        Some((x, y, 'L'))
                                    } else {
                                        None
                                    }
                                }
                                _ => unreachable!(),
                            })
                        })
                    })
                    .flatten()
                    .flatten()
                    .collect::<Vec<_>>();

                // note we effectively "integrate" across the difference between the top and the
                // bottom to find the area. there are some points to take into account here
                //   1. its perfectly valid for the vertical-line test to fail *so long as the
                //      interiors of the pieces are connected in some way*
                //   2. we can bail if there exists an interval on x s.t. the area is zero but the
                //      area computed so far isn't complete
                // it may be possible to apply sweepline et. al algorithms to this, but we would
                // need a drastically different representation of the underlying data (and,
                // considering this is a unit test to verify correctness, i operate on an
                // optimize-as-needed basis)

                // todo actually do the thing
                panic!("feas_reg: {:#?}", &feas_reg);
                todo!()
            })];
        }

        #[ignore]
        #[test]
        fn test_general_connected_2() {
            test_general_connected::<2>();
        }
        #[ignore]
        #[test]
        fn test_general_connected_3() {
            test_general_connected::<3>();
        }
        #[ignore]
        #[test]
        fn test_general_connected_4() {
            test_general_connected::<4>();
        }
    }
}

#[cfg(test)]
mod bench {
    use super::*;

    macro_rules! bench_slice_state {
        ($length:expr) => {
            paste! {
                #[bench]
                fn [<bench_slice_state_ $length>](b: &mut test::Bencher) {
                    b.iter(|| {
                        Grid::<$length>::new_slice_state();
                    });
                }
            }
        };
    }

    macro_rules! bench_slice_relation {
        ($length:expr) => {
            paste! {
                #[bench]
                fn [<bench_slice_relation_ $length>](b: &mut test::Bencher) {
                    let slice_state = Grid::<$length>::new_slice_state();
                    b.iter(|| {
                        Grid::<$length>::new_slice_relation(&slice_state);
                    });
                }
            }
        };
    }

    macro_rules! bench_solve {
        ($length:expr, $depth:expr) => {
            paste! {
            #[bench]
            fn [<bench_solve_ $length _ $depth>](b: &mut test::Bencher) {
                b.iter(|| Grid::<$length>::new().solve($depth));
            }
            }
        };
    }

    macro_rules! bench_all {
        ($length:expr) => {
            bench_slice_state! {$length}
            bench_slice_relation! {$length}
            bench_solve! {$length, 2}
            bench_solve! {$length, 3}
            bench_solve! {$length, 4}
            bench_solve! {$length, 5}
        };
    }

    // note i can throw some proc macros here but those need to be developed
    // as another crate (and its a bit pre-mature to do that now since these
    // are pushing the limit of reasonable execution times anyways)

    bench_all! {2}
    bench_all! {3}
    bench_all! {4}
}
