This is an open α version. You will have to build it yourself — no binary distribution is available yet. You will also need some special dependencies, but Cabal knows how to handle that.

Please share your experience with me at issue #1.

# `xgrep`

Cut through your XML with CSS selectors!

## introduction

```haskell
xgrep ∷ [Selector] → XML → Either [XML] CSV
```

`xgrep` is a command line tool, a filter that reads XML and writes either XML or CSV. You give it CSS selectors as arguments and it selects the matching cuts of the source XML for you.

### help

```
% xgrep --help
Usage: xgrep selectors [--columns] [--number-of-columns {number}] 
             [--delimiter {one byte character}]

  Cut through your XML with CSS selectors!

Available options:
  -h,--help                Show this help text
  --columns                Attempt to arrange the output in columns.
  --number-of-columns {number}
                           Specify number of columns.
  --delimiter {one byte character}
                           Specify delimiter.
```

## examples

### time

You want to know the current time according to the Internet. Presto:

```
% curl --silent 'https://www.timeanddate.com/worldclock/uk/london'  |  xgrep '#ct'
02:36:47
```

### table

You want to get some data from some web site. The data is conveniently presented as an HTML table. Presto:

```
% curl --silent 'https://pixel-prestige.com/crew-list.php'  |  xgrep 'tr.slim-row.crewRowItem td' --columns --number-of-columns 25  |  head
Admiral Snek	(Legendary)	Healing Rain	Leg, Accessory			55	0	35	88	72	86		10	1.6	3	10	9	9	28	9	40	1	2	110
AQ	(Legendary)	Gas	Leg, Weapon			86	0	47	43	83	61		12	2.9	2.7	8	15	15	14	15	70	1	2	110
Brightvoid Vector	(Legendary)	Gas	Weapon, Accessory		77	0	12	12	69	25		10	2.6	0.4	96	12	3	3	0	2	2	110
C.P.U.	(Legendary)	Stasis Shield	Body, Accessory			67	027	74	77	87		12	2.2	3	480	12	925	8	40	1	2	110
Cyber Duck	(Legendary)	Rush	Weapon, Accessory			74	91	72	100	83	65		14	1.2	1.2	50	12	22	27	17	100	2	3	110
Dracorpse	(Legendary)	Gas	Weapon, Accessory			95	0	5	5	83	28		14	3	0	70	0	0	0	1	1	1	110
Ember Empress	(Legendary)	Firewalk	Body, Weapon			69	0	0	0	0	0		12	3.2	0.4	60	8	8	6	8	100	1	2	110
Eva	(Legendary)	Urgent Repair	Weapon, Accessory			79	0	41	40	83	62		14	2.8	2.5	58	27	12	12	6	1	2	110
Galactic Alchemist	(Legendary)	Arson	Head, Accessory			100	0	52	86	76	61		12	2	2	1000	12	14	29	17	100	1	2	110
Galactic Snow Maiden	(Legendary)	Freeze	Head, Weapon			67	0	91	62	80	39		9	3.2	2.5	300	18	17	16	26	0	1	2	110
```
