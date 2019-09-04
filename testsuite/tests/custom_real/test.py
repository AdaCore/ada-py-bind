import gen

p = gen.Shape()
p.position.x = 0.52
p.position.y = -0.52

print p.position.x
print p.position.y

try:
    p.position.x = 1.2
except Exception as e:
    print e.message

print p.position.x
