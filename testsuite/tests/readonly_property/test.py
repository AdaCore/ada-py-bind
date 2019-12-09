import gen

p = gen.Point()

print p.x
try:
    p.x = 12
except Exception, e:
    print e
