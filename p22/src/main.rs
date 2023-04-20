type Input = (Vec<Vec<char>>, Vec<Move>);

#[derive(Debug, Clone, Copy)]
enum Facing {
    Up = 0,
    Right = 1,
    Down = 2,
    Left = 3,
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

fn write_position(pos: &Position, dbg_map: &mut Vec<Vec<char>>) {
    dbg_map[pos.x as usize][pos.y as usize] = match pos.facing {
        Facing::Down => 'V',
        Facing::Left => '<',
        Facing::Up => '^',
        Facing::Right => '>',
    };
}

fn print_dbg_map(map: &Vec<Vec<char>>) {
    for line in map {
        println!("{}", line.iter().collect::<String>());
    }
}

#[derive(Debug, Clone)]
struct CubeMap {
    side_length: i32,
    map: [Vec<Vec<char>>; 6],
    cube_mapping_info: [(i32, i32, Facing); 6],
}

impl CubeMap {
    fn from(
        map: &Vec<Vec<char>>,
        side_length: i32,
        cube_infos: [(i32, i32, Facing); 6],
    ) -> CubeMap {
        let mut cube_map = CubeMap {
            side_length,
            map: [vec![], vec![], vec![], vec![], vec![], vec![]],
            cube_mapping_info: cube_infos,
        };
        for face in 0..6 {
            for _ in 0..side_length {
                let mut v = Vec::new();
                for _ in 0..side_length {
                    v.push('.');
                }
                cube_map.map[face].push(v);
            }
        }
        for (face, (xorig, yorig, facing)) in cube_infos.iter().enumerate() {
            for x in (xorig * side_length)..(xorig * side_length + side_length) {
                for y in (yorig * side_length)..(yorig * side_length + side_length) {
                    let ch = map[x as usize][y as usize];
                    assert!(ch == '.' || ch == '#');
                    let x0 = x - xorig * side_length;
                    let y0 = y - yorig * side_length;
                    let (xrot, yrot) = match facing {
                        Facing::Up => (x0, y0),
                        Facing::Down => (side_length - 1 - x0, side_length - 1 - y0),
                        Facing::Left => (y0, side_length - 1 - x0),
                        Facing::Right => (side_length - 1 - y0, x0),
                    };
                    cube_map.map[face][xrot as usize][yrot as usize] = ch;
                }
            }
        }

        cube_map
    }

    fn get_at(&self, coords: &CubeCoords) -> char {
        self.map[coords.face as usize][coords.x as usize][coords.y as usize]
    }

    fn reproject(&self, coords: &CubeCoords) -> Position {
        let (face_x, face_y, face_facing) = self.cube_mapping_info[coords.face as usize];
        let (nx, ny) = match face_facing {
            Facing::Up => (coords.x, coords.y),
            Facing::Down => (
                self.side_length - 1 - coords.x,
                self.side_length - 1 - coords.y,
            ),
            Facing::Left => (self.side_length - 1 - coords.y, coords.x),
            Facing::Right => (coords.y, self.side_length - 1 - coords.x),
        };
        let (ox, oy) = (face_x * self.side_length, face_y * self.side_length);
        let new_facing = match (coords.facing as i8 + face_facing as i8).rem_euclid(4) {
            0 => Facing::Up,
            1 => Facing::Right,
            2 => Facing::Down,
            3 => Facing::Left,
            _ => panic!(),
        };
        Position {
            x: nx + ox,
            y: ny + oy,
            facing: new_facing,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct CubeCoords {
    x: i32,
    y: i32,
    facing: Facing,
    face: CubeFace,
}

impl CubeCoords {
    fn move_forward_one(&self, map: &CubeMap) -> CubeCoords {
        let side_length = map.side_length;
        let (xdelta, ydelta) = match self.facing {
            Facing::Up => (-1, 0),
            Facing::Down => (1, 0),
            Facing::Left => (0, -1),
            Facing::Right => (0, 1),
        };
        let mut next = self.clone();
        next.x += xdelta;
        next.y += ydelta;
        if next.x < 0 || next.x >= side_length || next.y < 0 || next.y >= side_length {
            next.face = match (self.face, self.facing) {
                (CubeFace::Top, Facing::Up) => CubeFace::Right,
                (CubeFace::Top, Facing::Down) => CubeFace::Left,
                (CubeFace::Top, Facing::Left) => CubeFace::Back,
                (CubeFace::Top, Facing::Right) => CubeFace::Front,
                (CubeFace::Front, Facing::Up) => CubeFace::Top,
                (CubeFace::Front, Facing::Down) => CubeFace::Bottom,
                (CubeFace::Front, Facing::Left) => CubeFace::Left,
                (CubeFace::Front, Facing::Right) => CubeFace::Right,
                (CubeFace::Right, Facing::Up) => CubeFace::Front,
                (CubeFace::Right, Facing::Down) => CubeFace::Back,
                (CubeFace::Right, Facing::Left) => CubeFace::Bottom,
                (CubeFace::Right, Facing::Right) => CubeFace::Top,
                (CubeFace::Back, Facing::Up) => CubeFace::Left,
                (CubeFace::Back, Facing::Down) => CubeFace::Right,
                (CubeFace::Back, Facing::Left) => CubeFace::Top,
                (CubeFace::Back, Facing::Right) => CubeFace::Bottom,
                (CubeFace::Bottom, Facing::Up) => CubeFace::Back,
                (CubeFace::Bottom, Facing::Down) => CubeFace::Front,
                (CubeFace::Bottom, Facing::Left) => CubeFace::Right,
                (CubeFace::Bottom, Facing::Right) => CubeFace::Left,
                (CubeFace::Left, Facing::Up) => CubeFace::Bottom,
                (CubeFace::Left, Facing::Down) => CubeFace::Top,
                (CubeFace::Left, Facing::Left) => CubeFace::Front,
                (CubeFace::Left, Facing::Right) => CubeFace::Back,
            };
            next.facing = match self.facing {
                Facing::Up => Facing::Left,
                Facing::Down => Facing::Up,
                Facing::Left => Facing::Right,
                Facing::Right => Facing::Down,
            };
            let along_side = match self.facing {
                Facing::Up => self.y,
                Facing::Down => side_length - 1 - self.y,
                Facing::Left => side_length - 1 - self.x,
                Facing::Right => self.x,
            };
            (next.x, next.y) = match next.facing {
                Facing::Up => (side_length - 1, along_side),
                Facing::Down => (0, side_length - 1 - along_side),
                Facing::Left => (side_length - 1 - along_side, side_length - 1),
                Facing::Right => (along_side, 0),
            };
        }
        next
    }

    fn make_move(&mut self, mov: &Move, map: &CubeMap, dbg_map: &mut Vec<Vec<char>>) {
        match mov {
            Move::Left => {
                self.facing = match self.facing {
                    Facing::Up => Facing::Left,
                    Facing::Down => Facing::Right,
                    Facing::Left => Facing::Down,
                    Facing::Right => Facing::Up,
                };
                let position = map.reproject(&self);
                write_position(&position, dbg_map);
            }
            Move::Right => {
                self.facing = match self.facing {
                    Facing::Up => Facing::Right,
                    Facing::Down => Facing::Left,
                    Facing::Left => Facing::Up,
                    Facing::Right => Facing::Down,
                };
                let position = map.reproject(&self);
                write_position(&position, dbg_map);
            }
            Move::Forward(iters) => {
                for _ in 0..*iters {
                    let next = self.move_forward_one(map);
                    if map.get_at(&next) == '#' {
                        break;
                    } else {
                        *self = next;
                        let position = map.reproject(&self);
                        write_position(&position, dbg_map);
                    }
                }
            }
        }
    }

    fn display(&self, map: &CubeMap) {
        let face = &map.map[self.face as usize];
        println!("Face: {:?}", self.face);
        for x in 0..map.side_length {
            let mut s = String::new();
            for y in 0..map.side_length {
                s.push(if x == self.x && y == self.y {
                    match self.facing {
                        Facing::Up => '^',
                        Facing::Down => 'v',
                        Facing::Left => '<',
                        Facing::Right => '>',
                    }
                } else {
                    face[x as usize][y as usize]
                })
            }
            println!("{}", s);
        }
    }
}

fn p2(input: &Input, side_length: i32) -> i32 {
    let mut dbg_map = input.0.clone();
    let original_map = &input.0;
    let moves = &input.1;
    let ((x, y, facing, face), cube_infos) = match side_length {
        4 => (
            (0, 3, Facing::Down, CubeFace::Top),
            [
                /* top */ (0, 2, Facing::Left),
                /* front */ (1, 0, Facing::Up),
                /* right */ (1, 1, Facing::Left),
                /* back */ (1, 2, Facing::Right),
                /* bottom */ (2, 2, Facing::Up),
                /* left */ (2, 3, Facing::Left),
            ],
        ),
        50 => (
            (0, 0, Facing::Up, CubeFace::Top),
            [
                /* top */ (0, 1, Facing::Up),
                /* front */ (0, 2, Facing::Left),
                /* right */ (3, 0, Facing::Down),
                /* back */ (2, 0, Facing::Up),
                /* bottom */ (2, 1, Facing::Left),
                /* left */ (1, 1, Facing::Down),
            ],
        ),
        _ => panic!(),
    };
    let cube_map = CubeMap::from(original_map, side_length, cube_infos);
    let mut position = CubeCoords { x, y, facing, face };
    for mov in moves {
        println!("{:?}", mov);
        position.make_move(mov, &cube_map, &mut dbg_map);
        position.display(&cube_map);
    }
    let projected_position = cube_map.reproject(&position);
    println!("{:?}", projected_position);
    print_dbg_map(&dbg_map);
    projected_position.score()
}

fn main() {
    let input_string = std::fs::read_to_string(std::env::args().nth(1).expect("Need input string"))
        .expect("Couldn't read file");
    let side_length = std::env::args()
        .nth(2)
        .expect("expect side length")
        .parse::<i32>()
        .unwrap();
    let input = parse(input_string);
    println!("{}", p1(&input));
    println!("{}", p2(&input, side_length));
}
