module Math

let rec gcd a b = if b = 0UL then a else gcd b (a % b)

let lcm a b = a * (b / gcd a b)

let quadratic a b c x =
    let a' = uint64 a
    let b' = uint64 b
    let c' = uint64 c
    let x' = uint64 x
    a'*x'*x' + b'*x' + c'

let modulo a b =
    if a < 0 then
        b + (a % b)
    else
        a % b
