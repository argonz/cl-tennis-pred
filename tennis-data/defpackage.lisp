(defpackage "TENNISDB"
  (:use "COMMON-LISP"
	#:masd
	"PROBABILITY-LIB"
	"IDHASH"
	"ITERATE")
  (:export #:+sets+
	   #:+set-ratios+
	   #:+3set-ratios+
	   #:+5set-ratios+
	   #:+game-results+
	   #:+current-date+
	   #:has-played?
	   #:participant?
	   #:direct-matches
	   #:sets-won?
	   "ID->PLAYER"
	   "PLAYERNAME->PLAYER"
	   "PLAYERID->PLAYERNAME"
	   "PLAYERNAME->PLAYERID"	   
	   "ALL-PLAYERS"
	   "ALL-PLAYERIDS"
	   "ID->MATCH"
	   "IDS->MATCHES"
	   "PLAYERID->MATCHIDS"
	   "PLAYERID->MATCHES"
	   "ALL-MATCHES"
	   "MATCHES-BETWEEN"
	   "COUNT-MATCHES"
	   "TRANSLATE-DATE"
	   "INITIALIZE-DB-FROM-DIF"
	   "COPY-TENNISDB"
	   "COPY-TENNISDB-DATE"))

(in-package "TENNISDB")

(export '(odds->prob
	  odds->rat
	  reverse-odds

	  opponent
	  opponents
	  reverse-sets
	  reverse-match
	  match-according
	  nr-of-sets
	  count-set
	  sets-according
	  match->setratio
	  odds-ext-according
	  odds-avg-according
	  won?
	  lost?
	  tight?
	  sets->game-result
	  winner-in-odds-bounds?
	  3set-game-sets?
	  3set-game? 
	  filter-matches-and
	  filter-matches-or
	  categorize-matches))