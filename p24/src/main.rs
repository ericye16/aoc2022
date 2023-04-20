use std::{
    collections::{HashSet, VecDeque},
    ops::{Deref, DerefMut},
};

#[derive(Debug, Clone, Copy)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn get_deltas(&self) -> (i32, i32) {
        match *self {
            Direction::Up => (-1, 0),
            Direction::Down => (1, 0),
            Direction::Left => (0, -1),
            Direction::Right => (0, 1),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Blizzard {
    x: i32,
    y: i32,
    direction: Direction,
}

struct Map(Vec<Vec<char>>);

impl Deref for Map {
    type Target = Vec<Vec<char>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Map {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Clone)]
struct MapSingleTime {
    blizzards: Vec<Blizzard>,
    occupied_map: Vec<Vec<char>>,
}

impl MapSingleTime {
    fn print_map(&self) {
        for line in &self.occupied_map {
            println!("{}", line.iter().collect::<String>());
        }
    }
}

fn populate_map(blizzards: &Vec<Blizzard>, map: &mut Vec<Vec<char>>) {
    for blizzard in blizzards {
        map[blizzard.x as usize][blizzard.y as usize] = 'o';
    }
}

impl MapSingleTime {
    fn next(&self) -> MapSingleTime {
        let mut next = self.clone();
        for line in &mut next.occupied_map {
            for ch in line.iter_mut() {
                *ch = match ch {
                    '#' => '#',
                    _ => '.',
                }
            }
        }
        let xlen = next.occupied_map.len() as i32;
        let ylen = next.occupied_map[0].len() as i32;
        for blizzard in &mut next.blizzards {
            let (xdelta, ydelta) = blizzard.direction.get_deltas();
            let (mut nx, mut ny) = (blizzard.x + xdelta, blizzard.y + ydelta);
            if nx == 0 {
                nx = xlen - 2;
            }
            if nx == xlen - 1 {
                nx = 1;
            }
            if ny == 0 {
                ny = ylen - 2;
            }
            if ny == ylen - 1 {
                ny = 1;
            }
            *blizzard = Blizzard {
                x: nx,
                y: ny,
                direction: blizzard.direction,
            };
        }
        populate_map(&next.blizzards, &mut next.occupied_map);
        next
    }
}

fn parse(inpt: &str, map: &mut Map) -> MapSingleTime {
    let mut blizzards = vec![];
    for (x, line) in inpt.lines().enumerate() {
        let mut v = vec![];
        for (y, char) in line.chars().enumerate() {
            let (ch, maybe_direction) = match char {
                '#' => ('#', None),
                '.' => ('.', None),
                '>' => ('.', Some(Direction::Right)),
                '<' => ('.', Some(Direction::Left)),
                '^' => ('.', Some(Direction::Up)),
                'v' => ('.', Some(Direction::Down)),
                _ => panic!("{char}"),
            };
            if let Some(direction) = maybe_direction {
                blizzards.push(Blizzard {
                    x: x as i32,
                    y: y as i32,
                    direction,
                });
            }
            v.push(ch);
        }
        map.push(v);
    }
    let mut occupied_map = map.clone();
    populate_map(&blizzards, &mut occupied_map);
    MapSingleTime {
        blizzards,
        occupied_map,
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum MoveState {
    Not,
    ReachedBottom,
    ReachedTop,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct Position {
    x: i32,
    y: i32,
    time_step: u32,
    moved_state: MoveState,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Problem {
    One,
    Two,
}

fn p1(inpt: &str, problem: Problem) -> i64 {
    let mut map = Map(vec![]);
    let first_map = parse(inpt, &mut map);
    let xlen = map.len() as i32;
    let ylen = map[0].len() as i32;
    let last_line = map.len() as i32 - 1;
    let mut y = None;
    for (y0, char) in map[0].iter().enumerate() {
        if *char == '.' {
            y = Some(y0);
            break;
        }
    }
    let mut visited = HashSet::new();
    let mut positions_queue = VecDeque::new();
    positions_queue.push_back(Position {
        x: 0,
        y: y.unwrap() as i32,
        time_step: 0,
        moved_state: MoveState::Not,
    });
    let mut maps = vec![first_map];
    let time_step = loop {
        let position = positions_queue.pop_back().unwrap();
        if visited.contains(&position) {
            continue;
        }
        visited.insert(position);
        if problem == Problem::One && position.x == last_line {
            break position.time_step;
        }
        if problem == Problem::Two
            && position.x == last_line
            && position.moved_state == MoveState::ReachedTop
        {
            break position.time_step;
        }
        let next_step = position.time_step + 1;
        // println!("{position:?}");
        if maps.len() as u32 <= next_step {
            maps.push(maps.last().unwrap().next());
            // println!("Trying step {}", next_step);
        }
        let next_map = &maps[next_step as usize];
        // next_map.print_map();
        for mov in [
            None,
            Some(Direction::Up),
            Some(Direction::Down),
            Some(Direction::Left),
            Some(Direction::Right),
        ] {
            let (xd, yd) = mov.map_or((0, 0), |f| f.get_deltas());
            let (nx, ny) = (position.x + xd, position.y + yd);
            // println!("{nx},{ny}");
            if nx >= 0
                && ny >= 0
                && nx < xlen
                && ny < ylen
                && next_map.occupied_map[nx as usize][ny as usize] == '.'
            {
                let next_state = if nx == 0 && position.moved_state == MoveState::ReachedBottom {
                    MoveState::ReachedTop
                } else if nx == last_line && position.moved_state == MoveState::Not {
                    MoveState::ReachedBottom
                } else {
                    position.moved_state
                };
                let pos = Position {
                    x: nx,
                    y: ny,
                    time_step: next_step,
                    moved_state: next_state,
                };
                // println!("{pos:?}");
                positions_queue.push_front(pos);
            }
        }
    };
    time_step.into()
}

fn main() {
    let input_string = std::fs::read_to_string(std::env::args().nth(1).expect("Need filename"))
        .expect("Couldn't read");

    println!("p1: {}", p1(&input_string, Problem::One));
    println!("p2: {}", p1(&input_string, Problem::Two));
}
