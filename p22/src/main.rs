type Input = (Vec<Vec<char>>, Vec<Move>);

#[derive(Debug, Clone, Copy)]
enum Facing {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Debug, Clone, Copy)]
struct Position {
    x: i32,
    y: i32,
    facing: Facing,
}

impl Position {
    fn score(&self) -> i32 {
        (self.x + 1) * 1000
            + (self.y + 1) * 4
            + match self.facing {
                Facing::Right => 0,
                Facing::Down => 1,
                Facing::Left => 2,
                Facing::Up => 3,
            }
    }
}

#[derive(Debug, Clone, Copy)]
enum Move {
    Left,
    Right,
    Forward(i32),
}

fn get_at(map: &Vec<Vec<char>>, pos: &Position) -> char {
    map[pos.x as usize][pos.y as usize]
}

// Next open OR closed
fn get_next(map: &Vec<Vec<char>>, pos: &Position) -> Position {
    let (xdelta, ydelta) = match pos.facing {
        Facing::Down => (1, 0),
        Facing::Up => (-1, 0),
        Facing::Left => (0, -1),
        Facing::Right => (0, 1),
    };
    let mut next = pos.clone();
    if xdelta == 0 {
        next.y = (next.y + ydelta).rem_euclid(map[next.x as usize].len() as i32);
        while get_at(map, &next) != '.' && get_at(map, &next) != '#' {
            next.y = (next.y + ydelta).rem_euclid(map[next.x as usize].len() as i32);
        }
    }
    if ydelta == 0 {
        next.x = (next.x + xdelta).rem_euclid(map.len() as i32);
        while next.y >= map[next.x as usize].len() as i32
            || (get_at(map, &next) != '.' && get_at(map, &next) != '#')
        {
            next.x = (next.x + xdelta).rem_euclid(map.len() as i32);
        }
    }
    next
}

fn p1(inp: &Input) -> i32 {
    let mut position = Position {
        x: 0,
        y: 0,
        facing: Facing::Right,
    };
    let map = &inp.0;
    let moves = &inp.1;
    // find first position
    for i in 0..map[0].len() {
        if map[0][i] == '.' {
            position.y = i as i32;
            break;
        }
    }
    for mov in moves {
        match *mov {
            Move::Left => {
                position.facing = match position.facing {
                    Facing::Down => Facing::Right,
                    Facing::Right => Facing::Up,
                    Facing::Up => Facing::Left,
                    Facing::Left => Facing::Down,
                }
            }
            Move::Right => {
                position.facing = match position.facing {
                    Facing::Down => Facing::Left,
                    Facing::Right => Facing::Down,
                    Facing::Up => Facing::Right,
                    Facing::Left => Facing::Up,
                }
            }
            Move::Forward(steps) => {
                for _ in 0..steps {
                    let next = get_next(&map, &position);
                    if get_at(&map, &next) == '#' {
                        break;
                    } else {
                        assert!(get_at(&map, &next) == '.');
                        position = next;
                    }
                }
            }
        }
        // println!("{position:?}");
    }
    position.score()
}

fn parse_moves(moves: &str) -> Vec<Move> {
    let s = moves.as_bytes();
    let mut v = Vec::new();
    let mut head = 0;
    let mut tail = 0;
    while head < s.len() {
        while tail < s.len() && (s[tail] != b'L' && s[tail] != b'R') {
            tail += 1;
        }
        let n = std::str::from_utf8(&s[head..tail])
            .unwrap()
            .parse::<i32>()
            .unwrap();
        v.push(Move::Forward(n));
        if tail < s.len() {
            v.push(if s[tail] == b'L' {
                Move::Left
            } else {
                Move::Right
            });
        }
        head = tail + 1;
        tail += 1;
    }
    v
}

fn parse(inp: String) -> Input {
    let lines = inp.lines();
    let mut v = Vec::new();
    let mut done_map = false;
    for line in lines {
        if line.is_empty() {
            done_map = true;
            continue;
        }
        if !done_map {
            v.push(line.chars().collect::<Vec<char>>());
        } else {
            return (v, parse_moves(line));
        }
    }
    panic!()
}

#[derive(Debug, Clone, Copy)]
enum CubeFace {
    Top,
    Front,
    Right,
    Back,
    Bottom,
    Left,
}

fn project_onto_cube(map: &Vec<Vec<char>>, old_pos: &Position) -> Position {
    assert!(map.len() % 4 == 0);
    assert!(map.len() % 50 != 0);
    // Can determine solely based on previous face and facing
    const SIDE_LENGTH: i32 = 4;
    let (net_side_x, net_side_y) = (old_pos.x / SIDE_LENGTH, old_pos.y / SIDE_LENGTH);
    let cube_face = match (net_side_x, net_side_y) {
        (0, 2) => CubeFace::Top,
        (1, 0) => CubeFace::Front,
        (1, 1) => CubeFace::Right,
        (1, 2) => CubeFace::Back,
        (2, 2) => CubeFace::Bottom,
        (2, 3) => CubeFace::Left,
        _ => panic!("Nonmatching cubeface"),
    };
    let new_position = match cube_face {
        CubeFace::Top => {
            match old_pos.facing {
                Facing::Left => {
                    // Top / left => right, facing down
                    assert!(old_pos.y == 2 * SIDE_LENGTH);
                    Position {
                        x: 1 * SIDE_LENGTH,
                        y: old_pos.x + 1 * SIDE_LENGTH,
                        facing: Facing::Down,
                    }
                }
                Facing::Right => {
                    // Top / right => left, facing left
                    assert!(old_pos.y == 3 * SIDE_LENGTH - 1);
                    Position {
                        x: 2 * SIDE_LENGTH + (SIDE_LENGTH - 1 - old_pos.x),
                        y: 4 * SIDE_LENGTH - 1,
                        facing: Facing::Left,
                    }
                }
                Facing::Up => {
                    // Top / up => front, facing down
                    assert!(old_pos.x == 0 * SIDE_LENGTH);
                    Position {
                        x: 1 * SIDE_LENGTH,
                        y: 3 * SIDE_LENGTH - 1 - old_pos.y,
                        facing: Facing::Down,
                    }
                }
                Facing::Down => {
                    panic!("Cannot down from top");
                }
            }
        }
        CubeFace::Front => {
            match old_pos.facing {
                Facing::Right => panic!("Cannot right from front"),
                Facing::Up => {
                    // Front / up => top, facing down
                    assert!(old_pos.x == 1 * SIDE_LENGTH);
                    Position {
                        x: 0,
                        y: 2 * SIDE_LENGTH + SIDE_LENGTH - 1 - old_pos.x,
                        facing: Facing::Down,
                    }
                }
                Facing::Down => {
                    // Front / down => bottom, facing up
                    assert!(old_pos.x == 2 * SIDE_LENGTH - 1);
                    Position {
                        x: 3 * SIDE_LENGTH - 1,
                        y: 2 * SIDE_LENGTH + (SIDE_LENGTH - 1 - old_pos.y),
                        facing: Facing::Up,
                    }
                }
                Facing::Left => {
                    // Front / left => left, facing up
                    assert!(old_pos.y == 0);
                    Position {
                        x: 3 * SIDE_LENGTH - 1,
                        y: 3 * SIDE_LENGTH + (SIDE_LENGTH - 1 - (old_pos.x - SIDE_LENGTH)),
                        facing: Facing::Up,
                    }
                }
            }
        }
        CubeFace::Right => {
            match old_pos.facing {
                Facing::Left => panic!("Cannot left from right"),
                Facing::Right => panic!("Cannot right from right"),
                Facing::Up => {
                    // Right / up => top, facing right
                    assert!(old_pos.x == 1 * SIDE_LENGTH);
                    Position {
                        x: old_pos.y - SIDE_LENGTH,
                        y: 2 * SIDE_LENGTH,
                        facing: Facing::Right,
                    }
                }
                Facing::Down => {
                    // Right / down => bottom, facing right
                    assert!(old_pos.x == 2 * SIDE_LENGTH - 1);
                    Position {
                        x: 2 * SIDE_LENGTH + (old_pos.y - SIDE_LENGTH),
                        y: 2 * SIDE_LENGTH,
                        facing: Facing::Right,
                    }
                }
            }
        }
        CubeFace::Back => {
            match old_pos.facing {
                Facing::Left => panic!("Cannot left from back"),
                Facing::Down => panic!("Cannot down front back"),
                Facing::Up => panic!("Cannot up from back"),
                Facing::Right => {
                    // Back / right => left, facing down
                    assert!(old_pos.y == 3 * SIDE_LENGTH - 1);
                    Position {
                        x: 2 * SIDE_LENGTH,
                        y: 3 * SIDE_LENGTH + (SIDE_LENGTH - 1 - (old_pos.x - 1 * SIDE_LENGTH)),
                        facing: Facing::Down,
                    }
                }
            }
        }
        CubeFace::Bottom => {
            match old_pos.facing {
                Facing::Up => panic!("Cannot up from bottom"),
                Facing::Right => panic!("cannot right from bottom"),
                Facing::Down => {
                    // Bottom / down => front, facing up
                    assert!(old_pos.x == 3 * SIDE_LENGTH - 1);
                    Position {
                        x: 2 * SIDE_LENGTH - 1,
                        y: SIDE_LENGTH - 1 - (old_pos.y - 2 * SIDE_LENGTH),
                        facing: Facing::Up,
                    }
                }
                Facing::Left => {
                    // Bottom / left => right, facing up
                    assert!(old_pos.y == 2 * SIDE_LENGTH);
                    Position {
                        x: 2 * SIDE_LENGTH - 1,
                        y: SIDE_LENGTH + (SIDE_LENGTH - 1 - (old_pos.x - 2 * SIDE_LENGTH)),
                        facing: Facing::Up,
                    }
                }
            }
        }
        CubeFace::Left => {
            match old_pos.facing {
                Facing::Left => panic!("Cannot left from left"),
                Facing::Down => {
                    // Left / down => front, facing right
                    assert!(old_pos.x == 3 * SIDE_LENGTH - 1);
                    Position {
                        x: SIDE_LENGTH + (SIDE_LENGTH - 1 - old_pos.y - 3 * SIDE_LENGTH),
                        y: 0,
                        facing: Facing::Right,
                    }
                }
                Facing::Right => {
                    // Left / right => top, facing left
                    assert!(old_pos.y == 4 * SIDE_LENGTH - 1);
                    Position {
                        x: SIDE_LENGTH - 1 - (old_pos.x - SIDE_LENGTH * 2),
                        y: 3 * SIDE_LENGTH - 1,
                        facing: Facing::Left,
                    }
                }
                Facing::Up => {
                    // Left / up => back, facing left
                    assert!(old_pos.x == 2 * SIDE_LENGTH);
                    Position {
                        x: SIDE_LENGTH + (SIDE_LENGTH - 1 - (old_pos.y - 3 * SIDE_LENGTH)),
                        y: 3 * SIDE_LENGTH - 1,
                        facing: Facing::Left,
                    }
                }
            }
        }
    };
    assert!(get_at(map, &new_position) == '.' || get_at(map, &new_position) == '#');
    new_position
}

// Next open OR closed
fn get_next_cube(map: &Vec<Vec<char>>, pos: &Position) -> Position {
    let (xdelta, ydelta) = match pos.facing {
        Facing::Down => (1, 0),
        Facing::Up => (-1, 0),
        Facing::Left => (0, -1),
        Facing::Right => (0, 1),
    };
    let mut next = pos.clone();
    next.x += xdelta;
    next.y += ydelta;
    if next.x < 0
        || next.x as usize >= map.len()
        || next.y < 0
        || next.y as usize >= map[next.x as usize].len()
        || get_at(map, &next) == ' '
    {
        // Cube block moving
        project_onto_cube(map, pos)
    } else {
        next
    }
}

fn write_position(pos: &Position, dbg_map: &mut Vec<Vec<char>>) {
    dbg_map[pos.x as usize][pos.y as usize] = match pos.facing {
        Facing::Down => 'V',
        Facing::Left => '<',
        Facing::Up => '^',
        Facing::Right => '>',
    };
    print_dbg_map(&dbg_map);
    println!();
}

fn print_dbg_map(map: &Vec<Vec<char>>) {
    for line in map {
        println!("{}", line.iter().collect::<String>());
    }
}

fn p2(inp: &Input) -> i32 {
    let mut position = Position {
        x: 0,
        y: 0,
        facing: Facing::Right,
    };
    let map = &inp.0;
    let mut dbg_map = map.clone();
    let moves = &inp.1;
    // find first position
    for i in 0..map[0].len() {
        if map[0][i] == '.' {
            position.y = i as i32;
            break;
        }
    }
    for mov in moves {
        println!("{mov:?}");
        match *mov {
            Move::Left => {
                position.facing = match position.facing {
                    Facing::Down => Facing::Right,
                    Facing::Right => Facing::Up,
                    Facing::Up => Facing::Left,
                    Facing::Left => Facing::Down,
                };
                write_position(&position, &mut dbg_map);
            }
            Move::Right => {
                position.facing = match position.facing {
                    Facing::Down => Facing::Left,
                    Facing::Right => Facing::Down,
                    Facing::Up => Facing::Right,
                    Facing::Left => Facing::Up,
                };
                write_position(&position, &mut dbg_map);
            }
            Move::Forward(steps) => {
                for _ in 0..steps {
                    let next = get_next_cube(&map, &position);
                    if get_at(&map, &next) == '#' {
                        break;
                    } else {
                        assert!(get_at(&map, &next) == '.');
                        position = next;
                    }
                    write_position(&position, &mut dbg_map);
                }
            }
        }
        println!("{position:?}");
    }
    position.score()
}

fn main() {
    let input_string = std::fs::read_to_string(std::env::args().nth(1).expect("Need input string"))
        .expect("Couldn't read file");
    let input = parse(input_string);
    // println!("{input:?}");
    println!("{}", p1(&input));
    println!("{}", p2(&input));
}
