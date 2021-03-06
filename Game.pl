:- style_check(-singleton).
:- dynamic current_node_is/1, equipped/2, located/2, health/2, defense/2, attack/2, magic_attack/2, magic_defense/2, prev_node/1, status/2,
    king_status/2, gold/1, potion_count/2, interactable/2.

/*
To play the game load this file in SWI-Prolog (or any equivalent)
and then type 'play.'
*/

different(X, X) :- !, fail.
different(X, Y).

/*
The players equipment.
*/
load_default_equipment :-
    assert(equipped(weapon_slot, nothing)),
    assert(equipped(armor_slot, nothing)),
    assert(equipped(head_slot, nothing)),
    assert(equipped(magic_slot, nothing)),
    assert(equipped(cape_slot, nothing)),
    assert(potion_count(health_potion, 0)),
    assert(gold(0)).

add_gold(I):- gold(X), NG is X + I, assert(gold(NG)), retract(gold(X)), format(" gold increased to : ~w", [NG]), nl.

/*
stats for characters
*/

load_default_player_stats :-
    assert(health(player, 50)),
    assert(defense(player, 1)),
    assert(attack(player, 1)),
    assert(magic_attack(player, 1)),
    assert(magic_defense(player, 1)).

%king_status(player, no).
health(slime, 15).
health(ghost, 15).
health(king, 50).
health(troll, 25).
health(skeleton, 20).

defense(slime, 1).
defense(ghost, 100).
defense(king, 5).
defense(troll, 3).
defense(skeleton, 4).

attack(slime, 1).
attack(ghost, 0).
attack(king, 7).
attack(troll, 3).
attack(skeleton, 1).

magic_attack(slime, 1).
magic_attack(ghost, 2).
magic_attack(king, 5).
magic_attack(troll, 0).
magic_attack(skeleton, 4).

magic_defense(slime, 1).
magic_defense(ghost, 1).
magic_defense(king, 5).
magic_defense(troll, 2).
magic_defense(skeleton, 4).

load_default_status :-
    assert(status(slime, alive)),
    assert(status(ghost, alive)),
    assert(status(player, alive)),
    assert(status(king, alive)),
    assert(status(troll, alive)),
    assert(status(skeleton, alive)).

/*
damage calculations
*/
deal_damage(X, Y) :- attack(X, AM), defense(Y, DM), health(Y, H), status(Y, alive), calc_damage(AM, DM, H, R), retract(health(Y, H)), assert(health(Y, R)),
 nl, format("~w has ~2f health after the attack from ~w", [Y, R, X]), nl, !.
deal_magic_damage(X, Y) :- magic_attack(X, AM), magic_defense(Y, DM), health(Y, H), status(Y, alive), calc_damage(AM, DM, H, R), retract(health(Y, H)), assert(health(Y, R)),
 nl, format("~w has ~2f health after the magic attack from ~w", [Y, R, X]), nl, !.

deal_damage_monster(X, Y) :- attack(X, AM), defense(Y, DM), magic_attack(X, MAM), magic_defense(Y, MDM), health(Y, H), status(Y, alive), calc_damage_physical(AM, DM, H, R1), calc_damage_magical(MAM, MDM, H, R2),
  R1 < R2, retract(health(Y, H)),assert(health(Y, R1)) , nl, 
  format("~w has ~2f health after the physical attack from ~w", [Y, R1, X]), nl, !.
deal_damage_monster(X, Y) :- attack(X, AM), defense(Y, DM), magic_attack(X, MAM), magic_defense(Y, MDM), health(Y, H), status(Y, alive), calc_damage_physical(AM, DM, H, R1), calc_damage_magical(MAM, MDM, H, R2),
  R1 > R2, retract(health(Y, H)),assert(health(Y, R2)) , nl, 
  format("~w has ~2f health after the magical attack from ~w", [Y, R2, X]), nl, !.
deal_damage_monster(X, Y) :- attack(X, AM), defense(Y, DM), magic_attack(X, MAM), magic_defense(Y, MDM), health(Y, H), status(Y, alive), calc_damage_physical(AM, DM, H, R1), calc_damage_magical(MAM, MDM, H, R2),
  R1 = R2, retract(health(Y, H)),assert(health(Y, R2)) , nl, 
  format("~w has ~2f health after the magical attack from ~w", [Y, R2, X]), nl, !.

calc_damage(AM, DM, H, R):-
 R is H - (10 * AM / DM).

calc_damage_physical(AM, DM, H, R):-
 R is H - (10 * AM / DM).

calc_damage_magical(MAM, MDM, H, R):-
 R is H - (10 * MAM / MDM).

/*
battle functions
*/
attack(Y):- deal_damage(player,Y), write("Player attacked!"), nl, deal_damage_monster(Y, player), write("Monster attacked!"), nl, check_dead(player), check_dead(Y).
magic_attack(Y):- deal_magic_damage(player,Y), deal_damage_monster(Y, player), check_dead(player), check_dead(Y).

check_dead(player):-  health(player, H), H < 1 , status(player, S), assert(status(player, dead)), retract(status(player, S)),
 nl, write("You are dead. Game Over."), nl, halt(1).

check_dead(X):- different(X, player), different(X, king), health(X, H), H < 1 , status(X, S), assert(status(X, dead)), retract(status(X, S)),
 nl, write(X), write(" is dead. You are safe to travel again."), nl, current_node_is(N), prev_node(PN), dead_event(X),
 assert(current_node_is(PN)), retract(current_node_is(N)), retract(prev_node(PN)), !.

check_dead(X):- different(X, player), health(X, H), H < 1, status(X, S), assert(status(X, dead)), retract(status(X, S)), current_node_is(N), prev_node(PN),
 assert(current_node_is(PN)), retract(current_node_is(N)), retract(prev_node(PN)), scene(PN),
 nl, write("The king is finally dead. Now go home and restore balance to your homeland!"), nl, !.

check_dead(_) :- !.

dead_event(slime):- add_potion(health_potion), add_gold(10).

dead_event(ghost):- add_gold(10).

dead_event(troll):- add_potion(health_potion), add_gold(20).
dead_event(_).

battle(X):- current_node_is(PN), status(X, alive), assert(prev_node(PN)), different(battle_dimension, PN),
 assert(current_node_is(battle_dimension)), retract(current_node_is(PN)), describe_battle(player, X).

describe_battle(X, Y):- health(X, H1), health(Y, H2), nl, 
    format("You have ~0f health remaining.", [H1]), nl,
    format("~w has ~0f health remaining.", [Y, H2]), nl,
    write("your options are attack("), write(Y), write(") and magic_attack("), write(Y), write(")"), nl, !.
/*
events for each location.
*/

event(slime_field):- 
status(slime, alive), write("You are attacked by a slime!"), nl, sleep(2), nl, battle(slime), !. 

event(river) :-
status(troll, alive), write("You come to a bridge that spans the river, but a troll jumps out to block your way!"), nl, sleep(2), nl, battle(troll), !.

event(tower) :-
status(skeleton, alive), write("After climbing the spiral staircase to the top of the tower, the skeletal remains of an ancient wizard attacks you!"), nl, sleep(2), nl, battle(skeleton), !.

event(boss):-
status(king, alive), write("You enter the throne room; the evil king is waiting with his broadsword at the ready. He is large and cunning, you will need all your strength to defeat him."), nl,
sleep(2), nl, write("You lunge forward to engage the evil king in battle!"), nl, battle(king), !.

event(_) :- !.



/*
edge(Start, Direction, Destination)
Represents an edge from the current node, travelling in the given direction and reaching the specified end node. 
*/

edge(home, north, crossroads).

edge(castle, south, home) :- equipped(head_slot, crown). % fast travel for end of game

edge(crossroads, north, forest) :- equipped(weapon_slot, short_sword), equipped(armor_slot, chainmail).
edge(crossroads, north, forest) :- equipped(weapon_slot, halberd), equipped(armor_slot, chainmail).
edge(crossroads, north, forest) :- write("It would be suicide to enter the forest without armor and weapons!"), nl, !, fail.
edge(crossroads, west, abandoned_house).
edge(crossroads, east, armory).

edge(forest, south, crossroads).
edge(forest, north, river).
edge(forest, west, shop).
edge(shop, east, forest).

edge(river, north, castle).
edge(river, south, forest).
edge(castle, south, river).
edge(castle, north, boss).
edge(boss, south, castle).
edge(castle, east, tower).
edge(tower, west, castle).

edge(abandoned_house, east, crossroads).
edge(abandoned_house, west, cemetery).
edge(cemetery, east, abandoned_house).


edge(armory, west, crossroads).
edge(armory, south, slime_field).
edge(slime_field, north, armory).

edge(slime_field, east, cave) :- located(halberd, weapon_slot), write("It would mean sure death to go back into that cave."), nl, !, fail.
edge(slime_field, east, cave) :- located(halberd, narnia), write("It would mean sure death to go back into that cave."), nl, !, fail.
edge(slime_field, east, cave).
edge(cave, west, slime_field).


map :- current_node_is(X), nl, format("currently in ~w. ", [X]), nl.
map :- current_node_is(X), edge(X, north, N), nl, format("to the north is ~w.", [N]).
map :- current_node_is(X), edge(X, west, N), nl, format("to the west is ~w.", [N]).
map :- current_node_is(X), edge(X, south, N), nl, format("to the south is ~w.", [N]).
map :- current_node_is(X), edge(X, east, N), nl, format("to the east is ~w.", [N]).  
/*
Initialize the location of the many items in the game.
*/

load_default_locations :-
    assert(located(short_sword, abandoned_house)),
    assert(located(chainmail, armory)),
    assert(located(halberd, cave)),
    assert(located(crown, boss)),
    assert(located(spellbook, tower)),
    assert(located(pristine_cloak, tower)),
    assert(located(frost_wand, cemetery)),
    assert(located(rugged_cloak, cemetery)),
    assert(located(magic_staff, forest)),
    assert(located(broadsword, shop)),
    assert(located(battle_axe, shop)),
    assert(located(breastplate, shop)),
    assert(located(fire_wand, shop)),
    assert(located(quality_cloak, shop)).

% add health potions to shop or somewhere else

/*
Initialize the stats of each item
*/
item(short_sword, weapon_slot, attack(player, 3)).
item(chainmail, armor_slot, defense(player, 3)).
item(crown, head_slot, king_status(player, yes)).
item(spellbook, magic_slot, magic_attack(player, 10)).
item(halberd, weapon_slot, attack(player, 5)).
item(battle_axe, weapon_slot, attack(player, 7)).
item(frost_wand, magic_slot, magic_attack(player, 3)).
item(magic_staff, magic_slot, magic_attack(player, 5)).
item(broadsword, weapon_slot, attack(player, 10)).
item(breastplate, armor_slot, defense(player, 7)).
item(fire_wand, magic_slot, magic_attack(player, 7)).
item(rugged_cloak, cape_slot, magic_defense(player, 2)).
item(quality_cloak, cape_slot, magic_defense(player, 4)).
item(pristine_cloak, cape_slot, magic_defense(player, 5)).
item(health_potion, potion, 1).

item(nothing, weapon_slot, attack(player, 1)).
item(nothing, armor_slot, defense(player, 1)).
item(nothing, magic_slot, magic_attack(player, 1)).
item(nothing, cape_slot, magic_defense(player, 1)).

search :- current_node_is(X), nl, format("searching ~w for items. ", [X]).
search :- current_node_is(X), located(I, X), nl, format("~w", [I]).
search :- current_node_is(X), nl, write("found everything useful").
/*
Handles movement from node to node.
*/
move(D) :- current_node_is(S), edge(S, D, E), retract(current_node_is(S)), assert(current_node_is(E)), scene(E), event(E), nl, !.
move(_) :- write("Unable to move in that direction!"), nl, !, fail.

% Directions

n :- move(north).
e :- move(east).
s :- move(south).
w :- move(west).
%l :- move(west).
%r :- move(east).
%f :- move(north).

drink_health_potion:- 
    current_node_is(X), different(battle_dimension, X), potion_count(health_potion, C), C > 0, heal(30), Y is C - 1, 
    retract(potion_count(health_potion, C)), assert(potion_count(health_potion, Y)), !.
drink_health_potion:-
    current_node_is(battle_dimension), nl,
    write("Can't drink potions during combat!"), nl, !.

heal(X) :- 
    health(player, Y), Z is X + Y, Z < 51, retract(health(player, Y)), assert(health(player, Z)), nl,
    format("Your health points have increased from ~w to ~w!", [Y, Z]), nl, !.
heal(X) :- 
    health(player, Y), Z is X + Y, Z > 50, retract(health(player, Y)), assert(health(player, 50)), nl,
    format("Your health points have increased from ~w to 50!", [Y]), nl, !.
/*
Initialize the iteractables of the game.
*/

interactable(coffin, cemetery).


interact(X):- interactable(X, L), current_node_is(L), interaction_event(X), retract(interactable(X,L)).

interaction_event(coffin):- status(ghost, alive), write("the coffin cover is cold and heavy. you notice the surrounding area is filled with cobwebs, it must have been closed for a while. with all your strength you bearly manage to open it."),nl,
sleep(2), nl, write("A ghost emerges from the fog and attacks you!"), battle(ghost), !.



uncover :- current_node_is(X), nl, format("searching ~w for intresting things. ", [X]).
uncover :- current_node_is(X), interactable(I, X), nl, format("~w", [I]).
uncover :- current_node_is(X), nl, write("found everything of interest.").
/*
Handles picking up items.
*/
take(chest) :- add_gold(50), assert(located(halberd, narnia)), retract(located(halberd, cave)), !.
take(I1) :-
    located(I1, shop), current_node_is(shop), item(I1, Y, S), equipped(Y, I2), different(item(I1, Y, S), item(_,potion,_)),
    buy(I1),
    assert(located(I1, Y)), assert(equipped(Y, I1)), retract(located(I1, X)), retract(equipped(Y, I2)), item_event(I1, S)
    , nl, !.
take(I1) :- 
    located(I1, X), current_node_is(X), item(I1, Y, S), equipped(Y, I2), different(X, shop), different(item(I1, Y, S), item(_,potion,_)),
    assert(located(I1, Y)), assert(equipped(Y, I1)), retract(located(I1, X)), retract(equipped(Y, I2)), item_event(I1, S)
    , nl, !.
take(I1):- located(I1, X), current_node_is(X), item(I1, potion, S), add_potion(I1), retract(located(I1, X)), !.
take(_) :- write("Unable to acquire that item."), !.

/*
Item purchasing.
*/
buy(broadsword) :- gold(X), X > 19, Y is X - 20, assert(gold(Y)), retract(gold(X)).
buy(broadsword) :- write("You don't have enough gold!"), nl, fail.

buy(battle_axe) :- gold(X), X > 19, Y is X - 20, assert(gold(Y)), retract(gold(X)).
buy(battle_axe) :- write("You don't have enough gold!"), nl, fail.

buy(breastplate) :- gold(X), X > 29, Y is X - 30, assert(gold(Y)), retract(gold(X)).
buy(breastplate) :- write("You don't have enough gold!"), nl, fail.

buy(fire_wand) :- gold(X), X > 49, Y is X - 50, assert(gold(Y)), retract(gold(X)).
buy(fire_wand) :- write("You don't have enough gold!"), nl, fail.

buy(quality_cloak) :- gold(X), X > 29, Y is X - 30, assert(gold(Y)), retract(gold(X)).
buy(quality_cloak) :- write("You don't have enough gold!"), nl, fail.

/*
item events
*/

item_event(I, S):- functor(S, F, N), OldStat =.. [F, player, B] , assert(S), retract(OldStat),
 nl, format("Item Aqquired: ~w", [I]), nl
, format("Old stat: ~w", [OldStat]), nl ,
 format("new stat: ~w", [S]), !.

item_event(_, _) :- !.

add_potion(I):- item(I, potion, N), potion_count(I, C), NC is C + N, assert(potion_count(I, NC)), retract(potion_count(I, C)), nl
, write(I), format(" count increased to : ~w", [NC]), !.

/*
Describes the players gear.
*/

inspect(player) :-
    nl,
    gold(G),
    potion_count(health_potion, P),
    equipped(head_slot, X),
    equipped(weapon_slot, Y),
    equipped(armor_slot, Z),
    equipped(magic_slot, M),
    equipped(cape_slot, C),
    health(player, H),
    defense(player, D),
    attack(player, A),
    magic_attack(player, MA),
    magic_defense(player, MD),
    format("Head slot: ~w", [X]), nl,
    format("Armor slot: ~w", [Z]), nl,
    format("Cape slot: ~w", [C]), nl,
    format("Weapon slot: ~w", [Y]), nl,
    format("Magic slot: ~w", [M]), nl,
    format("You currently have ~2f HP, ~w Attack, ~w Defence, ~w Magic Attack and ~w Magic Defence.", [H, A, D, MA, MD]), nl,
    format("You are carrying ~w gold coins and ~w health potion(s).", [G, P]), nl, !.
inspect(health_potion) :-
    potion_count(I, C),
    C > 0,
    nl,
    write("A bright red health potion, it will heal a large amount of health. To use type 'drink_health_potion.'"),
    nl, !.
inspect(I1) :-
    located(I1, X), current_node_is(X), item(I1, Y, S1), equipped(Y, I2), item(I2, Y, S2), different(item(I1, Y, S), item(_,potion,_)),
    format("The item in your ~w has a value of ~w", [Y, S2]), nl,
    format("The item you see has a value of ~w", [S1]), nl, !.
inspect(_) :- write("You can't inspect that!"), nl, !.

/*
Describes the current location to the player
*/

scene(X) :- current_node_is(X), description(X).

look :- current_node_is(X), description(X), !.

description(home) :-
    equipped(head_slot, crown),
    nl,
    write("At last, you are home again. With the crown on your head, you raise your hands and new life spreads outward to all the land."), nl,
    write("You win! Thanks for playing!"), nl,
    halt(1).
description(home) :-
    nl,
    write("You look around at the chaos the evil king has brought to your homeland."), nl,
    write("The trees are blackened, the meadows turned to swamps, and the animals have deserted this place."), nl,
    write("You must defeat the king and take his crown, which contains the Crystal of Life."), nl,
    write("With the crown in hand you can finally restore balance to the land."), nl,
    write("To the north you see the distant crossroads, everywhere else around you is a thick forest."), nl.

description(crossroads) :-
    nl,
    write("You reach the crossroads, from here you can go in any direction."), nl,
    write("To the north lies the haunted forest, beyond which is the evil king's castle. It would be unwise to venture there unprepared."), nl,
    write("To the west you see an abandoned house, perhaps the previous tenant left some weapons behind?"), nl,
    write("To the south is your home, but your quest lies ahead of you."), nl,
    write("To the east you see an armory, there might still be supplies inside."), nl.


% East of Crossroads
description(armory) :-
    located(chainmail, armory),
    nl,
    write("You enter a decrepit old armory, out of the corner of you eye you spot a glint of metal."), nl,
    write("You discover [chainmail], this will definitely help you in your fight against the evil king."), nl,
    write("Enter 'take(chainmail).' to pick up the armor."), nl,
    write("There is a field to the south, and the crossroads are back to the west."), nl.
description(armory) :-
    equipped(armor_slot, chainmail),
    nl,
    write("There is a field to the south of the now empty armory, and the crossroads are to the west."), nl.

description(slime_field) :-
    located(halberd, cave),
    nl,
    write("This field has been infested with slimes since the spread of the king's chaos. You see a cave"), nl,
    write("to the east of the field's edge, or you can turn back north to the relative safety of the armory."), nl.
description(slime_field) :-
    nl,
    write("The cave is too dangerous to return to, the only way out from here is back north to the armory."), nl.

description(cave) :-
    nl,
    write("You enter the cave, it is dark but you can see enough to spot two valuable items on the ground."), nl,
    write("A treasure chest and a steel [halberd] lie on opposite sides of the cave's mouth."), nl,
    write("But you hear a deep roar echo from the depths of the cave, there is only time to take one item."), nl,
    write("Enter 'take(chest).' or 'take(halberd).' and then run to the west!"), nl. % Fixed


% West of Crossroads
description(abandoned_house) :-
    located(short_sword, abandoned_house),
    nl,
    write("Whoever lived in this house has not been here for a long time, they must have been driven out by dark creatures in this land."), nl,
    write("You spot a scabbard hung on the wall, with a [short_sword] inside. It may be rusty, but it's better than nothing."), nl,
    write("Enter 'take(short_sword).' to equip the weapon."), nl,
    write("To the west is a small cemetery covered in fog, and to the east is the crossroads."), nl.
description(abandoned_house) :-
    equipped(weapon_slot, short_sword),
    nl,
    write("Whoever lived in this house has not been here for a long time, they must have been driven out by dark creatures in this land."), nl,
    write("To the west is a small cemetery covered in fog, and to the east is the crossroads."), nl.

description(cemetery) :-
    located(frost_wand, cemetery),
    nl,
    write("An eerie fog has taken up residence around the deteriorating gravestones. Be on your guard."), nl,
    write("Inspecting the gravestones, you see a glimmering blue [frost_wand] leaning against one."), nl,
    write("Enter 'take(frost_wand)' to pick up the magic weapon."), nl.
description(cemetery) :-
    %equipped(magic_slot, frost_wand),
    nl,
    write("An eerie fog has taken up residence around the deteriorating gravestones. Be on your guard."), nl,
    write("The only way to go from here is back the way you come, to the east."), nl.

description(cemetery2) :-
    write("You leave the cemetery at once and go back to the old house."), nl,
    move(east).


% North of Crossroads
description(forest) :-
    located(magic_staff, forest),
    nl,
    write("In the middle of the dark forest, you see a huge, gnarled tree with a [magic_staff] carved out of its roots."), nl,
    write("Enter 'take(magic_staff)' to equip the item."), nl,
    write("To the west you can just see a small shopkeep through the trees, and to the north is a wide river between the forest and the castle."), nl.
description(forest) :-
    %equipped(magic_slot, magic_staff),
    nl,
    write("To the west you can just see a small shopkeep through the trees, and to the north is a wide river between the forest and the castle."), nl.

description(shop) :-
    located(fire_wand, shop),
    located(battle_axe, shop),
    located(broadsword, shop),
    located(breastplate, shop),
    located(quality_cloak, shop),
    write("You find a goblin selling his wares in the middle of a small clearing."), nl,
    write("For sale is a [broadsword] for 20 gold, a [battle_axe] for 20 gold, a [breastplate] for 30 gold, a [fire_wand] for 50 gold, and a [quality_cloak] for 30 gold"), nl,
    write("Enter 'take(___)' for any items you'd like to buy."), nl, % implement shop system?
    write("The only exit is back east to the forest."), nl.
description(shop) :-
    write("The goblin is nowhere to be seen, so the only thing to do is go back east."), nl.

description(river) :-
    status(troll, alive).
description(river) :-
    nl,
    write("With the troll vanquished, you can continue north across the bridge to reach the evil king's castle, which looms large in the distance."), nl.

description(castle) :-
    equipped(head_slot, crown),
    nl,
    write("You begin the journey home, tired from your quest but driven by the thought of bringing peace back to the land."),
    move(south).
description(castle) :-
    located(spellbook, tower),
    nl,
    write("The castle door is locked, but you find a side entrance to sneak into. You can continue forward to the throne room or take a right and look around in the wizard's tower."), nl,
    write("Enter 'n.' to continue forward to the throne room, or enter 'e.' to turn right and search the wizard's tower."), nl.
description(castle) :-
    %equipped(magic_slot, spellbook),
    nl,
    write("Your final battle is ahead of you. The evil king resides in the throne room just ahead. Steel your resolve and prepare to fight!"), nl,
    write("Enter 'n.' to go back to the castle and face your enemy."), nl.

description(tower) :-
    located(spellbook, tower),
    status(skeleton, alive).
description(tower) :-
    located(spellbook, tower),
    nl,
    write("The scattered bones reveal the skeleton's magical arsenal: a wizard's [spellbook]."), nl,
    write("Enter 'take(spellbook)' to equip the magical book."), nl,
    write("Enter 'w.' to leave the tower and return to the main castle grounds."), nl.
description(tower) :-
    %equipped(magic_slot, spellbook),
    nl,
    write("There is nothing left here but old vials and tattered robes."), nl,
    write("Enter 'w.' to leave the tower and return to the main castle grounds."), nl.


description(boss) :-
    status(king, dead),
    nl,
    write("The king drops to the ground, blood soaking his royal robes."), nl,
    write("You take his crown with the Crystal of Life and place it on your head."), nl,
    write("Now you must go south to your home and restore balance to the land!"), nl,
    assert(equipped(head_slot, crown)),
    retract(equipped(head_slot, nothing)).
description(boss).

/*
Starts the game.
*/

play :- assert(current_node_is(home)), options.

new :- 
    load_default_equipment, load_default_status, load_default_player_stats, load_default_locations,
    tutorial, description(home).

load :- load_game, retract(current_node_is(home)), !.

options :-
    nl,
    write("Welcome to - The Legend of Lelda: Zink's Awakening!"), nl,
    write("To begin you can either start a new game by typing 'new.'"), nl,
    write("Or you can load a previous save by typing 'load.'"), nl, !.

/*
Basic instructions for players
*/

tutorial :-
    nl,
    write("You will recieve a short description of each location when you arrive there, pay attention to what's presented."), nl,
    write("You can move in four directions. North (n), East (e), South (s), and West (w). To move simply type the shorthand for a direction followed by a period."), nl,
    write("For example typing 'n.' would move you North."), nl,
    write("Once you reach a new location a short description of what you observe will be given."), nl,
    write("If you miss this description simply type 'look.' to have it be shown again."), nl,
    write("to find all the paths you can take for an area and find your current location type 'map.'."), nl,
    write("to find all interesting things to interact with type uncover."), nl,
    write("to interact with things of interest type interact(X)."), nl,
    write("to find all items in your current location type 'search.'."), nl,
    write("To pick up or equip an item type 'take(itemName)' where itemName can be any object the game describes such as a sword or armor."), nl,
    write("To inspect your current equipment type 'inspect(player).', to compare items type 'inspect(itemName).'"), nl,
    write("That's all for now. Enjoy the game!"), nl, !.

/*
Handles saving game state
*/

save :- current_node_is(battle_dimension), nl, write("Can't save during battles!"), nl, !.
save :- save_unary_misc, save_binary_misc, save_equipped, save_status, save_located.

save_unary_misc :- current_node_is(X), gold(Y), csv_write_file('save/save_unary_misc.txt', [current_node_is(X), gold(Y)]).

save_binary_misc :- health(player, X), defense(player, Y), attack(player, Z), magic_attack(player, A), magic_defense(player, B), potion_count(health_potion, C),
    csv_write_file('save/save_binary_misc.txt', 
        [health(player, X), defense(player, Y), attack(player, Z), magic_attack(player, A), magic_defense(player, B), potion_count(health_potion, C)]).

save_equipped :- findall(equipped(X, Y), equipped(X, Y), L), csv_write_file('save/save_equipped.txt', L).

save_status :- findall(status(X, Y), status(X, Y), L), csv_write_file('save/save_status.txt', L).

save_located :- findall(located(X,Y), located(X, Y), L), csv_write_file('save/save_located.txt', L).

/*
Handles loading game state
*/

load_game :- 
    current_node_is(home), 
    catch(see('save/save_unary_misc.txt'), E, (write('No save found'), fail_load)), 
    load_unary_misc, load_binary_misc, load_equipped, load_status, load_located.
load_game :- nl, write("Failed to load. Either no save files exist, or you already started your adventure."), nl, !.

fail_load :- nl, play, fail.

load_unary_misc :- csv_read_file('save/save_unary_misc.txt', X), load_unary_misc_state(X).
load_unary_misc_state([row(X), row(Y)]) :- assert(current_node_is(X)), assert(gold(Y)).

load_binary_misc :- csv_read_file('save/save_binary_misc.txt', X), load_binary_misc_state(X).
load_binary_misc_state([row(X1, Y1), row(X2, Y2), row(X3, Y3), row(X4, Y4), row(X5, Y5), row(X6, Y6)]) :- 
    assert(health(X1, Y1)), assert(defense(X2, Y2)), assert(attack(X3, Y3)), assert(magic_attack(X4, Y4)), assert(magic_defense(X5, Y5)), assert(potion_count(X6, Y6)).

load_equipped :- csv_read_file('save/save_equipped.txt', X), load_equipped_state(X).
load_equipped_state([row(X, Y)|T]) :- assert(equipped(X, Y)), load_equipped_state(T).
load_equipped_state([]).

load_status :- csv_read_file('save/save_status.txt', X), load_status_state(X).
load_status_state([row(X, Y)|T]) :- assert(status(X, Y)), load_status_state(T).
load_status_state([]).

load_located :- csv_read_file('save/save_located.txt', X), load_located_state(X).
load_located_state([row(X, Y)|T]) :- assert(located(X, Y)), load_located_state(T).
load_located_state([]).
