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

fn p1(inp: Input) -> i32 {
    let mut position = Position {
        x: 0,
        y: 0,
        facing: Facing::Right,
    };
    let map = inp.0;
    let moves = inp.1;
    // find first position
    for i in 0..map[0].len() {
        if map[0][i] == '.' {
            position.y = i as i32;
            break;
        }
    }
    for mov in moves {
        match mov {
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
        println!("{position:?}");
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

fn main() {
    let input_string = std::fs::read_to_string(std::env::args().nth(1).expect("Need input string"))
        .expect("Couldn't read file");
    let input = parse(input_string);
    // println!("{input:?}");
    println!("{}", p1(input));
    // let map = pparse(&input_string);
    // println!("p1: {}", p1(&map));
    // println!("p2: {}", p2(&map));
}
