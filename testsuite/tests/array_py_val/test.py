import gen

p = gen.Plot()

p.points[1].coords[1] = 12
p.points[1].coords[2] = 11
p.points[1].coords[3] = 10

print p.points[1].coords[1]
print p.points[1].coords[2]
print p.points[1].coords[3]
