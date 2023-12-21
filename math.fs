module Math

let rec gcd a b = if b = 0UL then a else gcd b (a % b)

let lcm a b = a * (b / gcd a b)
