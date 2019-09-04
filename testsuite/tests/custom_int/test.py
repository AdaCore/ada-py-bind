import gen

p = gen.Shape()
p.position.x = 490
p.position.y = 400

print p.position.x
print p.position.y

try:
    p.position.x = 2000
except Exception as e:
    print e.message

print p.position.x
