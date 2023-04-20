use std::{
    collections::HashSet,
    fmt::Display,
    ops::{Add, Deref, DerefMut, Index, IndexMut},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Position(i64, i64);

impl Add for Position {
    type Output = Position;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0, self.1 + rhs.1)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct Positions {
    value: BiHashMap<usize, Position>,
}

impl Deref for Positions {
    type Target = BiHashMap<usize, Position>;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl DerefMut for Positions {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl Positions {
    fn get_extremes(&self) -> Option<(Position, Position)> {
        if self.len() == 0 {
            None
        } else {
            Some((
                self.iter()
                    .map(|tuple| *tuple.1)
                    .reduce(|left, right| {
                        Position(
                            std::cmp::min(left.0, right.0),
                            std::cmp::min(left.1, right.1),
                        )
                    })
                    .unwrap(),
                self.iter()
                    .map(|tuple| *tuple.1)
                    .reduce(|left, right| {
                        Position(
                            std::cmp::max(left.0, right.0),
                            std::cmp::max(left.1, right.1),
                        )
                    })
                    .unwrap(),
            ))
        }
    }
}

impl Display for Positions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (mins, maxes) = self.get_extremes().unwrap();
        for x in mins.0..=maxes.0 {
            for y in mins.1..=maxes.1 {
                write!(
                    f,
                    "{}",
                    if let Some(idx) = self.get_by_right(&Position(x, y)) {
                        format!("{:02}", idx)
                    } else {
                        ". ".to_string()
                    }
                )?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

fn parse(inpt: &str) -> Positions {
    let mut p: Positions = Default::default();
    let mut idx = 0;
    for (x, line) in inpt.lines().enumerate() {
        for (y, ch) in line.chars().enumerate() {
            if ch == '#' {
                assert!(p.insert(idx, Position(x as i64, y as i64)) == bimap::Overwritten::Neither);
                idx += 1;
            }
        }
    }
    p
}

#[derive(Debug, Clone, Copy)]
enum Neighbor {
    North,
    NorthEast,
    East,
    SouthEast,
    South,
    SouthWest,
    West,
    NorthWest,
}

use bimap::BiHashMap;
use Neighbor::*;

impl Neighbor {
    fn to_delta(&self) -> Position {
        match *self {
            North => Position(-1, 0),
            NorthEast => Position(-1, 1),
            East => Position(0, 1),
            SouthEast => Position(1, 1),
            South => Position(1, 0),
            SouthWest => Position(1, -1),
            West => Position(0, -1),
            NorthWest => Position(-1, -1),
        }
    }

    pub fn iterator() -> impl Iterator<Item = Neighbor> {
        [
            North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest,
        ]
        .iter()
        .copied()
    }

    fn next(&self) -> Neighbor {
        match *self {
            North => South,
            South => West,
            West => East,
            East => North,
            _ => panic!("Invalid for next {self:?}"),
        }
    }

    fn neighbor_neighbors(&self) -> [Neighbor; 3] {
        match *self {
            North => [NorthWest, North, NorthEast],
            South => [SouthWest, South, SouthEast],
            West => [NorthWest, West, SouthWest],
            East => [NorthEast, East, SouthEast],
            _ => panic!("Invalid for neighbor_neighbors {self:?}"),
        }
    }
}

#[derive(Debug, Clone)]
struct NeighborsArray<T> {
    neighbors: [T; 8],
}

impl<T> Index<Neighbor> for NeighborsArray<T> {
    type Output = T;

    fn index(&self, index: Neighbor) -> &Self::Output {
        &self.neighbors[index as usize]
    }
}

impl<T> IndexMut<Neighbor> for NeighborsArray<T> {
    fn index_mut(&mut self, index: Neighbor) -> &mut Self::Output {
        &mut self.neighbors[index as usize]
    }
}

impl<T> Deref for NeighborsArray<T> {
    type Target = [T; 8];

    fn deref(&self) -> &Self::Target {
        &self.neighbors
    }
}

impl<T> DerefMut for NeighborsArray<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.neighbors
    }
}

fn find_proposal(
    current_position: Position,
    first_direction: Neighbor,
    positions: &Positions,
) -> Position {
    let mut neighbors_exist = NeighborsArray {
        neighbors: [false; 8],
    };
    for neighbor in Neighbor::iterator() {
        let neighbor_position = current_position + neighbor.to_delta();
        neighbors_exist[neighbor] = positions.contains_right(&neighbor_position);
    }
    if !neighbors_exist.iter().any(|b| *b) {
        current_position
    } else {
        let mut direction_to_consider = first_direction;
        for _ in 0..4 {
            let neighbors_of_neighbors = direction_to_consider.neighbor_neighbors();
            let neighbors_free = !neighbors_of_neighbors
                .iter()
                .map(|neighbor| neighbors_exist[*neighbor])
                .any(|b| b);
            if neighbors_free {
                return current_position + direction_to_consider.to_delta();
            }
            direction_to_consider = direction_to_consider.next();
        }
        current_position
    }
}

fn p1(positions: &Positions, num_rounds: Option<i32>) -> i64 {
    let mut positions = positions.clone();
    let mut facing = North;
    // println!("Initial state");
    // println!("{}", positions);
    let mut round = 0;
    loop {
        // let pre_move = positions.clone();
        let mut proposed_map = BiHashMap::new();
        let mut conflicts = HashSet::new();
        for (idx, position) in &*positions {
            let proposed_position = find_proposal(*position, facing, &positions);
            let overwritten = proposed_map.insert(*idx, proposed_position);
            match overwritten {
                bimap::Overwritten::Right(l_idx, _) => {
                    conflicts.insert(*idx);
                    conflicts.insert(l_idx);
                }
                bimap::Overwritten::Neither => (),
                _ => panic!("ohno {overwritten:?}"),
            }
        }
        let mut moved = false;
        facing = facing.next();
        for (idx, position) in proposed_map {
            if conflicts.contains(&idx) {
                continue;
            }
            match positions.insert(idx, position) {
                bimap::Overwritten::Left(_, _) => moved = true,
                bimap::Overwritten::Pair(_, _) => (),
                _ => panic!(),
            }
        }
        println!("After round {}", round + 1);
        // println!("{}", positions);
        round += 1;
        if let Some(num_rounds) = num_rounds {
            if round >= num_rounds {
                break;
            }
        }
        if num_rounds.is_none() && !moved {
            return round.into();
        }
    }
    let (mins, maxes) = positions.get_extremes().unwrap();
    (maxes.0 - mins.0 + 1) * (maxes.1 - mins.1 + 1) - positions.len() as i64
}

fn main() {
    let input_string = std::fs::read_to_string(std::env::args().nth(1).expect("Need input string"))
        .expect("Couldn't read file");
    let input = parse(&input_string);
    println!("p1: {}", p1(&input, Some(10)));
    println!("p2: {}", p1(&input, None));
}
