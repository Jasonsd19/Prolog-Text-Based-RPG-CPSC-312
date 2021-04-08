:- dynamic current_node_is/1, equipped/2, located/2.

different(X, X) :- !, fail.
different(X, Y).

/* Ideas:
    -To save or load, the only informaiton we need is the location and equipment.
    -This is due to the level checks being based on equipment only.
    -Therefore our save files can just be text files, that we parse.

    -Have battle scenarios where we clear the terminal and show both
    -our health points and the enemy health points.
    -We can choose an attack from a menu and then calculate damage
    -and refresh the terminal with updated helth values, repeat for enemy.
*/

/* The players current location. */
current_node_is(home).

/*
The players equipment.
*/

equipped(weapon_slot, nothing).
equipped(armor_slot, nothing).
equipped(head_slot, nothing).


/*
edge(Start, Direction, Destination)
Represents an edge from the current node, travelling in the given direction and reaching the specified end node. 
*/

edge(home, north, crossroads).

edge(crossroads, north, maze) :- equipped(weapon_slot, sword), equipped(armor_slot, armor).
edge(crossroads, north, maze) :- write("It would be suicide to enter the maze without armor and weapons!"), nl, fail.
edge(crossroads, east, abandoned_house).
edge(crossroads, south, home).
edge(crossroads, west, armory).

edge(maze, south, crossroads).

edge(abandoned_house, west, crossroads).

edge(armory, east, crossroads).

/*
Initialize the location of the many items in the game.
*/

located(sword, abandoned_house).
located(armor, armory).
located(crown, boss).

/*
Initialize the enemies in the game. Not used in the demo (used demo as proxy for determining whether or not boss is alive or dead).
*/

is_alive(boss).

/*
Handles movement from node to node.
*/
move(D) :- current_node_is(S), edge(S, D, E), assert(current_node_is(E)), retract(current_node_is(S)), scene, nl, !.
move(_) :- write("Unable to move in that direction!"), nl, !, fail.

% Directions

n :- move(north).
e :- move(east).
s :- move(south).
w :- move(west).

/*
Handles picking up items.
*/
take(I1) :- 
    located(I1, X), equipped(Y, I2), different(I1, I2), different(X, Y), 
    assert(located(I1, Y)), assert(equipped(Y, I1)), retract(located(I1, X)), retract(equipped(Y, I2)),
    write("You have equipped the item!"), nl, !.
take(_) :- write("The item has already been taken!"), !.


/*
Describes the players gear.
*/

inspect :-
    nl,
    equipped(head_slot, X),
    equipped(weapon_slot, Y),
    equipped(armor_slot, Z),
    format("Head slot: ~w", [X]), nl,
    format("Armor slot: ~w", [Y]), nl,
    format("Weapon slot: ~w", [Z]), nl, !.

/*
Describes the current location to the player
*/

scene :- current_node_is(X), description(X).

description(home) :-
    equipped(head_slot, crown),
    nl,
    write("You stand outside your home. Crown on your head, you know rule the land!"), nl,
    write("You win! Thanks for playing! Press '.' to exit."), nl,
    read(_),
    halt(0).
description(home) :-
    nl,
    write("You stand outside your home. You are prepared to begin your adventure to obtain the crown!"), nl,
    write("To the north you see the distant crossroads, everywhere else around you is a thick forest."), nl.

description(crossroads) :-
    nl,
    write("You reach the crossroads, here you are able to access many places."), nl,
    write("To the north you see the haunted maze, where the evil king resides. It would be unwise to venture there unprepared."), nl,
    write("To the east you see an abandoned house, perhaps the previous tenant left some weapons behind?"), nl,
    write("To the south you see your home, there is not much for you to gain by moving backwards!"), nl,
    write("To the west you see an armory, there might still be supplies inside."), nl.

description(maze) :-
    equipped(head_slot, crown),
    nl,
    write("The evil king lays slain on the ground, his lifeblood slowly soaking into the earth."), nl,
    write("You should return home quickly.").
description(maze) :-
    nl,
    write("The evil king stands before you, crown on head! The time to fight is now!"), nl,
    write("The king strikes first, but his blow bounces off of your armor."), nl,
    write("Responding in kind, you thrust at the king with a deft stroke..."), nl,
    write("And bury the sword deep within his heart! The crown is yours!"), nl,
    write("Now return to your home to claim your rightful spot as the king!"), nl,
    assert(equipped(head_slot, crown)),
    retract(equipped(head_slot, nothing)).

description(armory) :-
    equipped(armor_slot, armor),
    nl,
    write("You enter a decrepid old armory, out of the corner of you eye you spot a glint of metal."), nl.
description(armory) :-
    nl,
    write("You enter a decrepid old armory, there doesn't seem to be anything of importance here."), nl,
    write("You discover [armor], this will definitely help you in your fight against the evil king."), nl.

description(abandoned_house) :-
    equipped(weapon_slot, sword),
    nl,
    write("It seems warriors used to inhabit this house, but now nothing remains but dust and dirt."), nl.
description(abandoned_house) :-
    nl,
    write("It seems warriors used to inhabit this house, but now nothing remains but dust and dirt."), nl,
    write("Although you spot a scabbard hung on the wall, with a [sword] inside. It may be rusty, but something is better than nothing."), nl.

/*
Starts the game.
*/

play :- tutorial, scene.

/*
Basic instructions for players
*/

tutorial :-
    nl,
    write("Welcome to - The Legend of Lelda: Zink's Awakening!"), nl,
    write("You will recieve a short description of each location when you arrive there, pay attention to what's presented."), nl,
    write("You can move in four direction. North (n), East (e), South (s), and West (w). To move simply type the shorthand for a direction followed by a period."), nl,
    write("For example typing 'n.' would move you North."), nl,
    write("To pick up or equip an item type 'take(itemName)' where itemName can be any object the game describes such as a sword or armor."), nl,
    write("To inspect your current equipment type 'inspect.'"), nl,
    write("That's all for now. Enjoy the game!"), nl.
