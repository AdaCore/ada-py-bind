import gen

p = gen.Shape()
pos = gen.Point()

def show_error(fn):
    print "In show_error:"
    try:
        fn()
    except TypeError as e:
        print "    " + e.message
    else:
        print "    OK"


show_error(lambda: p.translate())
show_error(lambda: p.translate("lol"))
show_error(lambda: p.translate(move=pos, pouet=pos))
show_error(lambda: p.translate(pos))
show_error(lambda: p.translate(move=pos))
show_error(lambda: gen.Shape("loul"))
