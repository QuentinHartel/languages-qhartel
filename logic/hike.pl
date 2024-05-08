:- dynamic likes/2.
:- dynamic need/1.
:- dynamic difficulty/1.
:- dynamic snacks/1.

difficulty(high).
difficulty(medium).
difficulty(low).

needs(water).
needs(hikingSticks).

snacks(granolaBars).
snacks(fruitSnacks).
snacks(vegetables).

likes(hope, low).
likes(hope, medium).
likes(hope, high).
likes(hope, granolaBars).
likes(hope, fruitSnacks).

likes(lily, low).
likes(lily, medium).
likes(lily, granolaBars).
likes(lily, vegetables).
likes(lily, fruitSnacks).

like(Person,Thing) :- assert(likes(Person,Thing)).
dislike(Person, Thing) :- retract(likes(Person, Thing)).

dislike(hope, vegetables).
dislike(lily, high).

hike(Thing) :- difficulty(Thing), likes(lily, Thing), likes(hope, Thing).
take(Thing) :- snacks(Thing), likes(lily, Thing), likes(hope, Thing).
bring(Thing) :- (needs(Thing) -> needs(Thing); take(Thing)).