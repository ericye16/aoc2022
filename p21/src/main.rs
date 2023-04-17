use std::collections::HashMap;

use anyhow::Result;
use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug, Clone)]
enum Expr {
    Lit { v: i64 },
    Comb { l: String, r: String, op: char },
}

fn parse(s: &&str) -> (String, Expr) {
    lazy_static! {
        static ref LIT_REGEX: Regex = Regex::new(r"(\w+):\s*(\d+)").unwrap();
        static ref COMB_REGEX: Regex = Regex::new(r"(\w+):\s*(\w+) ([+*/-]) (\w+)").unwrap();
    }
    if let Some(captures) = LIT_REGEX.captures(s) {
        let name = captures.get(1).unwrap().as_str();
        let v = captures.get(2).unwrap().as_str().parse::<i64>().unwrap();
        return (name.to_string(), Expr::Lit { v });
    }
    if let Some(captures) = COMB_REGEX.captures(s) {
        let name = captures.get(1).unwrap().as_str();
        let l = captures.get(2).unwrap().as_str().to_string();
        let op = captures.get(3).unwrap().as_str().chars().next().unwrap();
        let r = captures.get(4).unwrap().as_str().to_string();
        return (name.to_string(), Expr::Comb { l, r, op });
    }
    panic!()
}

type MMap = HashMap<String, Expr>;

fn pparse(inp: &str) -> MMap {
    let inp = inp.split('\n').collect::<Vec<&str>>();
    let exprs = inp.iter().map(parse).collect::<Vec<(String, Expr)>>();
    let mut map = HashMap::new();
    for (s, e) in exprs {
        map.insert(s, e);
    }
    return map;
}

fn eval(map: &MMap, name: &str) -> i64 {
    let expr = map.get(name).unwrap();
    match expr {
        Expr::Lit { v } => *v,
        Expr::Comb { l, r, op } => {
            let l = eval(map, l);
            let r = eval(map, r);
            match op {
                '+' => l + r,
                '-' => l - r,
                '/' => l / r,
                '*' => l * r,
                _ => panic!(),
            }
        }
    }
}

fn p1(map: &MMap) -> i64 {
    return eval(&map, "root");
}

#[derive(Debug, Clone)]
enum MExpr {
    Humn,
    Lit(i64),
    Comb {
        l: Box<MExpr>,
        r: Box<MExpr>,
        op: char,
    },
}

fn eval_except_humn(map: &MMap, name: &str) -> MExpr {
    if name == "humn" {
        return MExpr::Humn;
    }
    let expr = map.get(name).unwrap();
    match expr {
        Expr::Lit { v } => MExpr::Lit(*v),
        Expr::Comb { l, r, op } => {
            let l = eval_except_humn(map, l);
            let r = eval_except_humn(map, r);
            if let (MExpr::Lit(l), MExpr::Lit(r)) = (&l, &r) {
                match op {
                    '+' => MExpr::Lit(l + r),
                    '-' => MExpr::Lit(l - r),
                    '/' => MExpr::Lit(l / r),
                    '*' => MExpr::Lit(l * r),
                    _ => panic!(),
                }
            } else {
                MExpr::Comb {
                    l: Box::new(l),
                    r: Box::new(r),
                    op: *op,
                }
            }
        }
    }
}

enum Side {
    Left,
    Right,
}

fn solve(mut lit: i64, symb: &mut MExpr) -> i64 {
    let mut solved = matches!(*symb, MExpr::Humn);
    while !solved {
        // println!("{lit:?} = {symb:?}");
        // we will be overwriting symb at the end, MExpr::Humn is just a placeholder
        match std::mem::replace(symb, MExpr::Humn) {
            MExpr::Humn => {
                solved = true;
            }
            MExpr::Comb { l, r, op } => {
                let (litval, symbside, side) = match (&*l, &*r) {
                    (MExpr::Lit(v), _) => (v, *r, Side::Left),
                    (_, MExpr::Lit(v)) => (v, *l, Side::Right),
                    _ => panic!(),
                };
                match op {
                    '+' => {
                        lit -= litval;
                    }
                    '-' => {
                        match side {
                            Side::Right => {
                                // 1 = x - 2, side = right
                                // => (1 + 2) = x
                                lit += litval;
                            }
                            Side::Left => {
                                // 1 = 2 - x, side = left
                                // => (1 - 2) = -x
                                // => -(1 - 2) = x
                                lit -= litval;
                                lit *= -1;
                            }
                        }
                    }
                    '*' => {
                        lit /= litval;
                    },
                    '/' => match side {
                        Side::Right => {
                            // 1 = x / 2
                            // 1 * 2 = x
                            lit *= litval;
                        }
                        Side::Left => {
                            // 1 = 2 / x
                            // 1 * x = 2
                            // x = 2 / 1
                            lit = litval / lit;
                        }
                    },
                    _ => panic!(),
                }
                *symb = symbside;
            }
            _ => panic!(),
        }
    }
    lit
}

fn p2(map: &MMap) -> i64 {
    let root = map.get("root").unwrap();
    let Expr::Comb { l, r, op: _ } = root else {panic!()};
    let lmmap = eval_except_humn(map, l);
    let rmmap = eval_except_humn(map, r);
    let (lit, mut symb) = match (lmmap, rmmap) {
        (MExpr::Lit(a), b) => (a, b),
        (b, MExpr::Lit(a)) => (a, b),
        _ => panic!(),
    };
    solve(lit, &mut symb)
}

fn main() -> Result<()> {
    let input_string = std::fs::read_to_string(std::env::args().nth(1).expect("Need input string"))
        .expect("Couldn't read file");
    let map = pparse(&input_string);
    println!("p1: {}", p1(&map));
    println!("p2: {}", p2(&map));
    Ok(())
}
