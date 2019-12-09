import gen

p = gen.Point()

print p.coords["x"]
print p.coords["y"]
print p.coords["z"]

p.coords["x"] = 12
p.coords["y"] = 13
p.coords["z"] = 14

print p.coords["x"]
print p.coords["y"]
print p.coords["z"]
