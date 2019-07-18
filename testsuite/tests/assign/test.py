import gen

p = gen.Shape()

for x, y in zip(range(1, 20), range(1, 40, 2)):
    p.position.x = x
    p.position.y = y
    print p.position.x
    print p.position.y

