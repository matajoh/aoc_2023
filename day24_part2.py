import sympy as sp

with open("inputs/day24.txt") as f:
    starts = []
    dirs = []
    for line in f:
        parts = line.split("@")
        starts.append(tuple([int(x.strip()) for x in parts[0].split(",")]))
        dirs.append(tuple([int(x.strip()) for x in parts[1].split(",")]))

x0, y0, z0 = starts[0]
dx0, dy0, dz0 = dirs[0]
x1, y1, z1 = starts[1]
dx1, dy1, dz1 = dirs[1]
x2, y2, z2 = starts[2]
dx2, dy2, dz2 = dirs[2]
x3, y3, z3 = starts[3]
dx3, dy3, dz3 = dirs[3]

a, b, c, d, e, f = sp.symbols("a b c d e f")

def abde(start, direction):
    x, y, _ = start
    dx, dy, _ = direction
    return (x-a) * (dy - e) - (y - b) * (dx - d)

eqs = [sp.simplify(abde(starts[i], dirs[i]) - abde(starts[i+1], dirs[i+1]))
       for i in range(0, 4)]

abde_solve = sp.solve(eqs, (a, b, d, e), dict=True)[0]

def acdf(start, direction):
    x, _, z = start
    dx, _, dz = direction
    return (x - a) * (dz - f) - (z - c) * (dx - d)

eqs = [sp.simplify(acdf(starts[i], dirs[i]) - acdf(starts[i+1], dirs[i+1]))
       for i in range(0, 4)]

acdf_solve = sp.solve(eqs, (a, c, d, f), dict=True)[0]

x, y, z = abde_solve[a], abde_solve[b], acdf_solve[c]
print(x + y + z)