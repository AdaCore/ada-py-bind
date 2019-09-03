import gen
import json

p = gen.from_json(json.dumps({"id": "lolwut", "x": 12, "y": 15}))
print p.id
print p.position.x
print p.position.y

p.position.x = 0
p.position.y = 2
p.id = "hahahoho"

print p.to_json()
