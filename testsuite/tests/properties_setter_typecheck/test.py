import gen

p = gen.Point()

p.x = 55

try:
    p.x = 55.5
except Exception, e:
    print e

try:
    p.x = "55.5"
except Exception, e:
    print e
