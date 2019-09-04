import gen

p = gen.Shape()
print p.kind
p.kind = "circle"
print p.kind

try:
    p.kind ="lol"
except Exception as e:
    print e.message

print p.kind
