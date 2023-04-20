fn snafu_digit(c: char) -> i64 {
    match c {
        '0' => 0,
        '1' => 1,
        '2' => 2,
        '-' => -1,
        '=' => -2,
        _ => panic!(),
    }
}

fn from_snafu(s: &str) -> i64 {
    let mut o = 0;
    for (idx, ch) in s.chars().rev().enumerate() {
        o += 5_i64.pow(idx.try_into().unwrap()) * snafu_digit(ch);
    }
    o
}

fn to_snafu(mut inp: i64) -> String {
    let mut s = String::new();
    while inp != 0 {
        let v = inp % 5;
        let (ch, carry) = match v {
            0 => ('0', false),
            1 => ('1', false),
            2 => ('2', false),
            3 => ('=', true),
            4 => ('-', true),
            _ => panic!(),
        };
        s.push(ch);
        if carry {
            inp += 5;
        }
        inp /= 5;
    }
    s.chars().rev().collect()
}

fn p1(inpt: &str) -> String {
    let sum = inpt.lines().map(from_snafu).sum();
    println!("{}", sum);
    to_snafu(sum)
}

fn main() {
    let input_string = std::fs::read_to_string(std::env::args().nth(1).expect("Need filename"))
        .expect("Couldn't read");

    println!("p1: {}", p1(&input_string));
}
