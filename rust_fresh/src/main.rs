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

// helper stuff
struct AddWithCarry {
    pub min: u8,
    pub max: u8,
    pub len: usize,
    pub val: Vec<u8>,
}

impl AddWithCarry {
    fn new(min: u8, max: u8, len: usize) -> Self {
        Self {
            min,
            max,
            len,
            val: vec![min; len],
        }
    }
}

impl Iterator for AddWithCarry {
    type Item = AddWithCarry;

    fn next(&mut self) -> Option<AddWithCarry> {
        let mut pos = 0;
        while pos < self.val.len() && self.val[pos] == self.max {
            self.val[pos] = self.min;
            pos += 1;
        }

        if pos < self.val.len() as usize {
            self.val[pos] += 1;
            Some(AddWithCarry {
                min: self.min,
                max: self.max,
                len: self.len,
                val: self.val.clone(),
            })
        } else {
            None
        }
    }
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
            % cur_id_to_len_byte.len()
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
        _cur_id_to_len_byte: &Vec<GridLen>,
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

        run_vec.iter().lt(run_vec.iter().rev())
            || (run_vec.iter().eq(run_vec.iter().rev())
                && cur_pos_to_id_byte
                    .iter()
                    .le(cur_pos_to_id_byte.iter().rev()))
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

        if let Some(_fail) = checks
            .iter()
            .find(|x| !(x.1)(&cur_id_to_len_byte, &cur_pos_to_id_byte))
        {
            dbgwrap!(
                "test {} failed for id_to_len: {:?}, pos_to_id: {:?}",
                _fail.0,
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
        format!["{}", a] == format!["{}", b]
    }

    fn reverse(mut a: GridSliceState) -> GridSliceState {
        a.pos_to_id.reverse();
        a
    }

    fn symmetric(&self) -> bool {
        Self::non_canonical_equal(&Self::reverse(self.clone()), &self)
    }

    fn pos_to_id(&self, flip: bool, pos: usize) -> u8 {
        if !flip {
            self.pos_to_id[pos as usize]
        } else {
            self.pos_to_id[self.pos_to_id.len() - pos as usize - 1]
        }
    }

    fn pos_to_id_iter<'a>(&'a self, flip: bool) -> Box<dyn Iterator<Item = &'a u8> + 'a> {
        if !flip {
            Box::new(self.pos_to_id.iter())
        } else {
            Box::new(self.pos_to_id.iter().rev())
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct GridSliceRelation {
    pub _func: (GridSliceState, GridSliceState),
    pub _piece_map: Vec<u8>,
    pub flip: bool,
}

impl GridSliceRelation {
    // note both of these functions are broken because a height of three is not a sufficient
    // condition for an upper border (look at the case of (l) in the paper), so we need to be
    // smarter about how we define borders between pieces

    fn pass_no_int_bord(
        x: &GridSliceState,
        y: &GridSliceState,
        piece_map: &Vec<u8>,
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

            let cross_left = piece_map[*x_id as usize] != y_id;
            let cross_right = x_pos == x.pos_to_id.len() - 1
                || piece_map[x.pos_to_id(flip, x_pos + 1) as usize]
                    != y.pos_to_id(false, x_pos + 1);
            let cross_up = x_pos == x.pos_to_id.len() - 1 || y_id != y.pos_to_id(false, x_pos + 1);
            let cross_down =
                x_pos == x.pos_to_id.len() - 1 || *x_id != x.pos_to_id(flip, x_pos + 1);

            cross_left as u8 + cross_right as u8 + cross_up as u8 + cross_down as u8 != 1
        })
    }

    fn pass_inc_vol(
        x: &GridSliceState,
        y: &GridSliceState,
        piece_map: &Vec<u8>,
        _flip: bool,
    ) -> bool {
        // the volume of a piece must increase by the number of times it occurs in the next slice.
        // we know a piece occurs in the next slice if the length is less than three and we map the
        // x piece id to the y piece id because the ids are only unique within a slice)

        piece_map.iter().enumerate().all(|a| {
            let x_id = a.0 as u8;
            let y_id = *a.1;

            if y_id != y.id_to_len.len() as u8 {
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
                x.id_to_len[x_id as usize] == x.id_to_len.len() as u8
                    || x.id_to_len[x_id as usize] == 0
            }
        }) && y.id_to_len.iter().enumerate().all(|a| {
            let y_id = a.0 as u8;
            let _y_len = *a.1;

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

    fn pass_map_valid_domain(
        x: &GridSliceState,
        y: &GridSliceState,
        piece_map: &Vec<u8>,
        _flip: bool,
    ) -> bool {
        x.id_to_len.iter().enumerate().all(|(x_id, x_len)| if *x_len == 0 || *x_len == x.id_to_len.len() as u8 { piece_map[x_id] == x.id_to_len.len() as u8 } else { true }) // if the length of a piece in x is zero or three, then it does not map (an x piece does not have to map)
                    && y.id_to_len.iter().enumerate().all(|(y_id, y_len)| if *y_len == 0 { piece_map.iter().filter(|y_id_cur| **y_id_cur == y_id as u8).count() == 0 } else { true })
        // if the length of a piece in y is zero, then it does not map
    }

    fn pass_connected(
        x: &GridSliceState,
        y: &GridSliceState,
        piece_map: &Vec<u8>,
        flip: bool,
    ) -> bool {
        // verify that each x<->y mapping have a connecting between them
        //
        // note: this might also not generalize beyond the 3 case

        piece_map.iter().enumerate().all(|a| {
            let x_id = a.0 as u8;
            let y_id = *a.1;

            // either the map doesn't apply or there exists a connection between the ids
            y_id == piece_map.len() as u8
                || (0..piece_map.len())
                    .any(|pos| x.pos_to_id(flip, pos) == x_id && y.pos_to_id[pos] == y_id)
        })
    }

    fn pass_unique_flip(
        x: &GridSliceState,
        y: &GridSliceState,
        _piece_map: &Vec<u8>,
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

    fn new(
        _len: GridLen,
        x: &GridSliceState,
        y: &GridSliceState,
        piece_map: &Vec<u8>,
        flip: bool,
    ) -> Result<Self, ()> {
        // note it might be a good idea to make these fn casts into a type alias
        let checks = [
            (
                "no int bord",
                Self::pass_no_int_bord
                    as fn(&GridSliceState, &GridSliceState, &Vec<u8>, bool) -> bool,
            ),
            (
                "inc vol",
                Self::pass_inc_vol as fn(&GridSliceState, &GridSliceState, &Vec<u8>, bool) -> bool,
            ),
            (
                "map valid domain",
                Self::pass_map_valid_domain
                    as fn(&GridSliceState, &GridSliceState, &Vec<u8>, bool) -> bool,
            ),
            (
                "connected",
                Self::pass_connected
                    as fn(&GridSliceState, &GridSliceState, &Vec<u8>, bool) -> bool,
            ),
            (
                "unique flip",
                Self::pass_unique_flip
                    as fn(&GridSliceState, &GridSliceState, &Vec<u8>, bool) -> bool,
            ),
        ];
        if let Some(_fail) = checks.iter().find(|a| !(a.1)(x, y, piece_map, flip)) {
            dbgwrap!(
                "test {} failed for {} -> {} ({:?} -> {:?}) with piece_map {:?} and flip {}",
                _fail.0,
                x,
                y,
                x,
                y,
                piece_map,
                flip
            );
            Err(())
        } else {
            Ok(GridSliceRelation {
                _func: (x.clone(), y.clone()),
                _piece_map: piece_map.clone(),
                flip,
            })
        }
    }
}

impl std::fmt::Display for GridSliceRelation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} -> {}",
            {
                let mut x = self._func.0.clone();
                x.pos_to_id.reverse();
                format!["{}", x]
            },
            self._func.1
        )
        .unwrap();
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct GridTiling {
    // note we don't keep GridSliceRelation because we don't persist the flip bit and other things
    // like that
    pub slice_relation_stack: Vec<(GridSliceState, GridSliceState, Vec<u8>)>,
}

impl GridTiling {
    fn new() -> Self {
        Self {
            slice_relation_stack: Vec::new(),
        }
    }

    fn push(&mut self, a: (GridSliceState, GridSliceState, Vec<u8>)) {
        self.slice_relation_stack.push(a);
    }

    fn pop(&mut self) {
        self.slice_relation_stack
            .truncate(self.slice_relation_stack.len() - 1);
    }

    fn peek(&self) -> (GridSliceState, GridSliceState, Vec<u8>) {
        self.slice_relation_stack[self.slice_relation_stack.len() - 1].clone()
    }

    fn iter(&self) -> std::slice::Iter<'_, (GridSliceState, GridSliceState, Vec<u8>)> {
        self.slice_relation_stack.iter()
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
        let mut cur_slice_pos = slice_start;
        let mut cur_piece_id = piece_id;

            // note that we map piece ids to global ids by a left/right position and a slice
            // relation, which implies one of two options. as a matter of convention we always use
            // the bottom-most id for the relation (i.e. we take the y of the first without a match
            // instead of the x of the last with a match)

            // while the current stack maps a previous id to the current id, go back
            while let Some(cur_map) = self.slice_relation_stack[cur_slice_pos]
                .2
                .iter()
                .enumerate()
                .find(|(cur_piece_x_id, cur_piece_y_id)| **cur_piece_y_id == cur_piece_id)
            {
                cur_slice_pos = cur_slice_pos - 1;
                cur_piece_id = cur_map.0 as u8;

                if cur_slice_pos == 0 {
                    break;
                }
            }

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

        let mut ret = Vec::new();
        let mut state = Vec::new();
        for (cur_slice_pos, cur_slice) in self.slice_relation_stack.iter().enumerate() {
            for (cur_piece_pos, cur_piece_id) in cur_slice.1.pos_to_id.iter().enumerate() {
                let cur_id = self.render_graph_backtrack_slice_piece_id(
                    &mut state,
                    cur_slice_pos,
                    *cur_piece_id,
                );

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
                                        &mut state,
                                        cur_slice_pos,
                                        cur_slice.1.pos_to_id[cur_piece_pos - 1],
                                    ),
                                ))
                            } else {
                                None
                            }
                        }
                        1 => {
                            // connect to right neighbor (if one exists)
                            if cur_piece_pos < cur_slice.1.pos_to_id.len() - 1 {
                                Some((
                                    cur_id,
                                    self.render_graph_backtrack_slice_piece_id(
                                        &mut state,
                                        cur_slice_pos,
                                        cur_slice.1.pos_to_id[cur_piece_pos + 1],
                                    ),
                                ))
                            } else {
                                None
                            }
                        }
                        2 => {
                            // connect to top neighbor (if one exists and is connected)
                            if cur_slice_pos > 0
                                && (cur_slice.2)[cur_slice.0.pos_to_id[cur_piece_pos] as usize]
                                    == *cur_piece_id
                            {
                                println!("connecting current slice state id to previous slice state id");
                                Some((
                                    cur_id,
                                    self.render_graph_backtrack_slice_piece_id(
                                        &mut state,
                                        cur_slice_pos - 1,
                                        cur_slice.0.pos_to_id[cur_piece_pos]
                                    ),
                                ))
                            } else {
                                println!("not connecting current slice state id to previous slice state id");
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
            }
        }
        ret.sort();
        ret.dedup_by(|a, b| (a.0 == b.0 && a.1 == b.1));

        (ret, state)
    }

    fn render_color(&self, graph: &Vec<(u8, u8)>, state: &Vec<(usize, u8, u8)>) -> Vec<u8> {
        // note graph coloring is an interesting problem but its outside the scope here. we have
        // four as an upper bound for the number of colors (because of the four-color theorem, but
        // it is easier to show in the square-grid case), so we brute-force the 1, 2, 3 and 4
        // colorings and return the first

        println!("rendering {:?} with color", graph);
        AddWithCarry::new(0, 4, state.len())
            .find(|color| {
                // verify that no two colors are connected by an edge
                graph
                    .iter()
                    .all(|edge| (color.val)[edge.0 as usize] != (color.val)[edge.1 as usize])
            })
            .unwrap()
            .val
    }

    fn render(&self) {
        println!("TILING");

        println!(
            "{:?}\t{}",
            self.slice_relation_stack[0].0, self.slice_relation_stack[0].0
        );
        self.slice_relation_stack
            .iter()
            .for_each(|y| println!("{:?}\t{}", y.1, y.1));
        let mut graph = self.render_graph();
        let color = self.render_color(&graph.0, &graph.1);

        println!("graph: {:?}", &graph);
        println!("color: {:?}", &color);

        self.slice_relation_stack.iter().enumerate().for_each(
            |(cur_slice_relation_pos, cur_slice_relation)| {
                cur_slice_relation.1.pos_to_id.iter().enumerate().for_each(
                    |(cur_piece_pos, cur_piece_id)| {
                        let global_id = self.render_graph_backtrack_slice_piece_id(
                            &mut graph.1,
                            cur_slice_relation_pos,
                            *cur_piece_id,
                        );

                        print!(
                            "\x1b[3{}m{}{}{}\x1b[39;49m",
                            match color[global_id as usize] {
                                0 => 1,
                                1 => 2,
                                2 => 3,
                                3 => 4,
                                _ => unreachable!(),
                            },
                            char::from_u32(0x00002588).unwrap(),
                            char::from_u32(0x00002588).unwrap(),
                            char::from_u32(0x00002588).unwrap(),
                        );
                    },
                );
                println!("");
            },
        )
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
        slice_state
            .iter()
            .flat_map(move |x| {
                slice_state.iter().flat_map(move |y| {
                    // note i should do all add_with_carry uses like this
                    AddWithCarry::new(0, len, len as usize).flat_map(move |piece_map| {
                        (0..2).flat_map(move |flip| {
                            GridSliceRelation::new(len, x, y, &piece_map.val, flip != 0)
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
        println!("finding slice: {}", a);
        self.slice_state
            .iter()
            .find(|x| format!["{}", x] == a)
            .unwrap()
            .clone()
    }

    fn solve_base(&self) -> (GridSliceState, GridSliceState, Vec<u8>) {
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

    fn solve_iter(&self, stack: &mut GridTiling, tiling: &mut Vec<GridTiling>) {
        if stack.len() as u8 == self._len + 3 {
            let prev = stack.peek();
            let base = self.solve_base();

            if GridSliceState::non_canonical_equal(&prev.0, &base.0)
                && GridSliceState::non_canonical_equal(&prev.1, &base.1)
            {
                tiling.push(stack.clone());
            }
        } else {
            let prev = stack.peek().1.clone();

            println!("STACK");
            stack.iter().for_each(|x| println!("{}\n{}\n", x.0, x.1));

            self.slice_relation
                .iter()
                // we generate the valid flips (sets the parity, effectively)
                .map(|x| {
                    (
                        if x.flip {
                            GridSliceState::reverse(x._func.0.clone()).clone()
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
                    println!("recursing on {}", x.1);

                    stack.push(x);
                    self.solve_iter(stack, tiling);
                    stack.pop();
                });
        }
    }

    fn solve(&self) -> Vec<GridTiling> {
        let mut stack = GridTiling::new();
        let mut tiling = Vec::new();

        stack.push(self.solve_base());
        self.solve_iter(&mut stack, &mut tiling);

        println!("tilings (length = {})", tiling.len());
        tiling.iter().enumerate().for_each(|x| {
            println!("TILING {}", x.0);

            println!(
                "{:?}\t{}",
                x.1.slice_relation_stack[0].0, x.1.slice_relation_stack[0].0
            );
            x.1.slice_relation_stack
                .iter()
                .for_each(|y| println!("{:?}\t{}", y.1, y.1));
        });

        tiling
    }
}

fn main() {
    match std::env::args().nth(1).unwrap().as_str() {
        "ss" => {
            let grid = Grid::new(3);
            grid.slice_state.iter().for_each(|x| println!("{}", x))
        }
        "sr" => {
            let grid = Grid::new(3);
            grid.slice_relation.iter().for_each(|x| println!("{}", x))
        }
        "render" => {
            let grid = Grid::new(3);
            let solve = grid.solve();

            println!("tiling count: {}", solve.len());

            solve.iter().for_each(|x| x.render());
        }
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use itertools::Itertools;

    mod tetrominoes {
        use super::*;

        #[test]
        fn test_tetrominoes_basic() {
            /*
            GridSliceState::new(4, &vec![0, 4, 4, 4], &vec![3, 2, 2, 1]).unwrap();
            let grid = Grid::new(4);
            let a = grid.find_slice_by_render("| 4    4    4    4 |");
            let b = grid.find_slice_by_render("| 4 || 4    4 || 4 |");
            let c = grid.find_slice_by_render("| 3 || 2    2 || 3 |");
            let d = grid.find_slice_by_render("| 2 || 4    4 || 2 |");
            let e = grid.find_slice_by_render("| 1 || 2    2 || 1 |");

            // GridSliceRelation::new(4, &a, &b, todo!(), false);
            */
        }
        #[test]
        fn test_tetrominoes_slice_state() {
            let grid = Grid::new(4);
            grid.slice_state
                .iter()
                .enumerate()
                .for_each(|(i, x)| println!("{}: {}", i, x));

            // note the paper assumes that A = A' = A'' in the 3 case, and i'm generalizing the
            // trend to any slice state with all pieces of length 4 are equal (i.e. any state
            // whose next state is flat)

            let slice_state_len = grid.slice_state.len();
            let slice_state_equiv_corr_len = grid
                .slice_state
                .iter()
                .filter(|x| x.id_to_len.iter().any(|y| *y != 4 && *y != 0))
                .count()
                + 1;
            let slice_relation_len = grid.slice_relation.len();
            let grid_solve_len = grid.solve();

            dbg![(
                slice_state_len,
                slice_state_equiv_corr_len,
                slice_relation_len,
                grid_solve_len
            )];
            assert![false];
        }
    }

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

                #[test]
                fn test_trominoes_general_slice_state_basic() {
                    let a = GridSliceState::new(3, &vec![0, 0, 3], &vec![2, 2, 2]).unwrap();
                    println!("{}", &a);

                    assert![a.symmetric()];
                }

                fn get_reference() -> Vec<GridSliceState> {
                    // note: the paper says seven slice states because it has A = A' = A''
                    vec![
                        GridSliceState::new(3, &vec![0, 0, 3], &vec![2, 2, 2]).unwrap(), // A
                        GridSliceState::new(3, &vec![0, 3, 3], &vec![1, 2, 2]).unwrap(), // A'
                        GridSliceState::new(3, &vec![3, 3, 3], &vec![0, 1, 2]).unwrap(), // A''
                        GridSliceState::new(3, &vec![1, 2, 3], &vec![0, 1, 2]).unwrap(), // B
                        GridSliceState::new(3, &vec![1, 2, 3], &vec![1, 0, 2]).unwrap(), // C
                        GridSliceState::new(3, &vec![1, 2, 3], &vec![0, 2, 1]).unwrap(), // D
                        GridSliceState::new(3, &vec![2, 2, 2], &vec![0, 1, 2]).unwrap(), // E
                        GridSliceState::new(3, &vec![1, 1, 1], &vec![0, 1, 2]).unwrap(), // F
                        GridSliceState::new(3, &vec![0, 1, 2], &vec![1, 2, 2]).unwrap(), // G
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

                fn get_reference() -> Vec<GridSliceRelation> {
                    let slice_state = Grid::new_slice_state(3);
                    let find = |a: &'static str| -> &GridSliceState {
                        slice_state.iter().find(|x| format!["{}", x] == a).unwrap()
                    };

                    [
                        ("| 1 || 1 || 1 |", "| 2 || 2 || 2 |", vec![0, 1, 2], false),
                        ("| 1 || 2    2 |", "| 1 || 3 || 2 |", vec![3, 1, 2], true),
                        ("| 1 || 2    2 |", "| 2 || 1 || 3 |", vec![3, 1, 2], false),
                        ("| 1 || 2    2 |", "| 3 || 3    3 |", vec![3, 2, 1], true),
                        ("| 2 || 2 || 2 |", "| 3 || 3 || 3 |", vec![0, 1, 2], false),
                        ("| 3    3    3 |", "| 1 || 1 || 1 |", vec![3, 3, 3], false),
                        ("| 3    3    3 |", "| 1 || 2    2 |", vec![3, 3, 3], false),
                        ("| 3    3    3 |", "| 3    3    3 |", vec![3, 3, 3], false),
                        ("| 1 || 3 || 2 |", "| 2 || 1 || 3 |", vec![1, 2, 3], false),
                        ("| 1 || 3 || 2 |", "| 3 || 3    3 |", vec![2, 1, 3], true),
                        ("| 2 || 1 || 3 |", "| 1 || 2 || 3 |", vec![1, 2, 3], true),
                        ("| 2 || 1 || 3 |", "| 3 || 3    3 |", vec![2, 1, 3], false),
                        ("| 1 || 2 || 3 |", "| 1 || 3 || 2 |", vec![1, 2, 3], true),
                        ("| 3 || 3    3 |", "| 1 || 1 || 1 |", vec![3, 3, 3], false),
                        ("| 3 || 3    3 |", "| 1 || 2    2 |", vec![3, 3, 3], false),
                        ("| 3 || 3    3 |", "| 1 || 2    2 |", vec![3, 3, 3], true),
                        ("| 3 || 3    3 |", "| 3    3    3 |", vec![3, 3, 3], false),
                        ("| 3 || 3 || 3 |", "| 1 || 1 || 1 |", vec![3, 3, 3], false),
                        ("| 3 || 3 || 3 |", "| 1 || 2    2 |", vec![3, 3, 3], false),
                        ("| 3 || 3 || 3 |", "| 3    3    3 |", vec![3, 3, 3], false),
                    ]
                    .iter()
                    .map(|x| {
                        let ret =
                            GridSliceRelation::new(3, find(x.0), find(x.1), &x.2, x.3).unwrap();
                        ret
                    })
                    .collect::<Vec<_>>()
                }

                #[test]
                fn test_trominoes_general_slice_relation_reference() {
                    get_reference();
                }
                #[test]
                fn test_trominoes_general_slice_relation() {
                    let grid = Grid::new(3);

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
                fn test_trominoes_general_solve() {
                    let grid = Grid::new(3);
                    assert_eq![grid.solve(), 10];
                }
            }
        }
    }
}
