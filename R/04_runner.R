# Frage 4 (30P)

# Sie implementieren das Spiel "Runner".
# Sie spielen das Spiel mit einem Kartendeck aus vier Farben ("rot", "blau", "grün", "gelb"),
# die jeweils Karten mit Zahlen von eins bis 13 enthalten.
# Das Spiel kann mit einem Geber ("Dealer") und einem bis fünf Spielern gespielt werden.
# Das Spiel beginnt, wenn der Geber eine Karte vom Stapel zieht.
# Er fragt den ersten Spieler, ob die aktuelle Karte eine höhere,
# niedrigere oder gleiche Zahl als die vorherige Karte hat.
# Der Spieler erhält 1 Punkt, wenn er richtig liegt, und 1 Minuspunkt, wenn er falsch liegt.
# Die Karte, die der vorherige Spieler erraten musste, ist nun die Karte,
# die der nächste Spieler erraten muss.
# Das Spiel geht weiter, bis keine Karte mehr im Stapel ist.
# Ihre Funktionen werden automatisiert getestet.
# Die Objekte, die Sie in a) und b) erstellen können Sie auch testen.
# Fehlermeldungen aus den Beispielen müssen Sie nicht exakt replizieren.
# Diese geben aber Aufschluss darüber, was Sie abfangen müssen.

# a)
# Erstellen Sie einen `data.frame`, das als Datenbasis dient,
# um alle 52 Karten im Deck anzuzeigen, so dass jede Karte eindeutig identifiziert werden kann.
# Nennen Sie das `data.frame` "ex04adeck".
# Nennen Sie die Spalten "colour" und "number".
ex04adeck <- NULL

# b)
# Dies ist Ihr Starter Deck.
# Aus technischen Gründen müssen wir auch die aktuelle Karte und die Historie der Karten festhalten.
# Dieser Slot wird zunächst aber leer bleiben.
# Erstellen Sie eine Liste namens "ex04bdeck",
# die diese Einträge mit den Namen "card", "deck" und "history" enthält.
# Da wir noch nicht gespielt haben, sollten "card" und "history" leer sein.
# Das Element deck soll das in der Voraufgabe erstellte Deck enthalten.
ex04bdeck <- NULL

# c)
# Implementieren Sie ein Ziehen aus dem Deck als eine `R` Funktion.
# Ihre Funktion sollte eine benannte Liste zurückgeben,
# bei der der erste Eintrag die Reihe ist, die aus dem Deck gezogen wurde,
# und der zweite Eintrag das verbleibende Deck ohne die gezogene Karte ist.
# Der Verlauf (history) entspricht allen Karten, die aus dem Deck gezogen wurden,
# in chronologischer (absteigender) Reihenfolge.
# Alle Einträge sollten, sofern nicht leer, den gleichen Typ/die gleiche Klasse haben.
# Die Einträge sollten "card" und "deck", "history" genannt werden.
# Ignorieren Sie zunächst die "history" und erfassen Sie diese im Ziehen nicht.
# Nennen Sie die Funktion `ex04draw`.
# Sie benutzt das "Deck" als Eingabe und hat ein optionales Startargument.
# Die Eingabe "deck" sollte das gleiche Format haben wie die Ausgabe.
# Stellen Sie sicher, dass der Seed nur gesetzt wird, wenn er angegeben wird,
# so dass die Funktion standardmäßig zufällig arbeitet.
# Verwenden Sie die bereitgestellte Signatur.
# Ihnen stehen die Tests "4.31", "4.32" und "4.33" zur Verfügung.

# Eingabe
# `deck`: Eine Liste, die den Anforderungen für ein Deck aus b) entspricht.
# `seed`: Numerisches Skalar. Der Seed für das Generieren von Zufallszahlen.

# Ausgabe
# Ein `deck`, mit einer Karte weniger in `$deck` im Vergleich zum Input.
# Die entfernte Karte wird in `$card` als ein `data.frame` mit einer Zeile dargestellt.

# Beispiele
# example_deck <- list(
#  card = NULL,
#  deck = data.frame(colour = c("black", "orange", "white", "black"),
#                    number = c(8, 9, 1, 3)),
#  history = NULL)
# it does not matter which card is actually transferred
# ex04draw(example_deck, seed = 8L)
#> $card
#>  colour number
#>4  black      3
#>
#>$deck
#>  colour number
#>1  black      8
#>2 orange      9
#>3  white      1
#>
#>$history
#>NULL

# example_deck <- list(
#  card = NULL,
#  deck = data.frame(colour = c("black", "orange", "white", "black"),
#                    number = c(8, 9, 1, 3),
#                    irrelevant = c("A", "A", "C", "D")),
#  history = NULL)
# ex04draw(example_deck, seed = 8L)
#> $card
#>  colour number irrelevant
#>4  black      3          D
#>
#>$deck
#>  colour number irrelevant
#>1  black      8          A
#>2 orange      9          A
#>3  white      1          C
#>
#>$history
#>NULL

# example_deck <- list(
#  card = data.frame(colour = "green", number = 12),
#  deck = data.frame(colour = c("black", "orange", "white", "black"),
#                    number = c(8, 9, 1, 3)),
#  history = NULL)
# ex04draw(example_deck, seed = 8L)
#> $card
#>  colour number
#>4  black      3
#>
#>$deck
#>  colour number
#>1  black      8
#>2 orange      9
#>3  white      1
#>
#>$history
#>NULL

ex04draw <- function(deck, seed = NULL) {
#TODO
}

# d)
# Implementieren Sie die Entscheidung eines spielenden Individuums,
# das seine Entscheidung auf die vorherige (vor Beginn des Spiels) Verteilung der Zahlen stützt,
# d.h. das Individuum wird immer "größer" spielen, wenn eine Zahl kleiner als 7
# erscheint und "kleiner", wenn sie größer ist.
# Im Falle von 7 ist die Person unentschieden und trifft eine zufällige Entscheidung
# zwischen den gleich wahrscheinlichen Ereignissen (aus ihrer Sicht).
# Die Funktion nimmt die `card` (die Karte, über die entschieden werden soll) und
# das `deck` (das verbleibende Deck zum Zeitpunkt der Entscheidung) als Eingaben.
# Nennen Sie die Funktion `ex04decide_prior`.
# Die Funktion gibt einen skalaren Zeichenwert aus: "larger", "equal" oder smaller"
# (die getroffene Entscheidung).
# Verwenden Sie die bereitgestellte Signatur.
# Ihnen stehen die Tests "4.41", "4.42" und "4.43" zur Verfügung.

# Eingabe
# `card`: Ein `data.frame` mit einer Zeile und (mindestens) den zwei Spalten `colour` und `number`
# `deck`: Ein `data.frame` mit beliebig vielen Zeilen und (mindestens) den zwei Spalten `colour` und `number`

# Ausgabe
# Ein skalerer Character: entweder "smaller" oder "larger" gemäß der Angabe.

# Beispiele
# example_deck <- list(
#  card = data.frame(colour = "green", number = 12),
#  deck = data.frame(colour = c("black", "orange", "white", "black"),
#                    number = c(8, 9, 1, 3)),
#  history = NULL)
# ex04decide_prior(card = example_deck$card, deck = example_deck$deck)
#> [1] "smaller"

# example_deck <- list(
#  card = data.frame(colour = "green", number = 4),
#  deck = data.frame(colour = c("black", "orange", "white", "black"),
#                    number = c(8, 9, 1, 3)),
#  history = NULL)
# ex04decide_prior(card = example_deck$card, deck = example_deck$deck)
#> [1] "larger"

ex04decide_prior <- function(card, deck) {
  #TODO
}

# e)
# Vervollständigen Sie die nachstehende Funktion "reward",
# die die Punkte für eine bestimmte Entscheidung (`decision`)und die Zahl,
# die Sie wählen (`new_number`) , mit der Zahl der offenen Karte (`old_number`) vergleicht.
# Ihnen stehen die Tests "4.51", "4.52" und "4.53" zur Verfügung.

# Eingabe
# `decision`: Ein skalarer character, die Entscheidung,
# die vor dem Zug der neuen Karte getroffen wurde.
# Muss entweder "larger", "smaller" oder "equal" sein.
# `new_number`: Ein skalarer Numeric, die Nummer der neu gezogenen Karte
# `old_number`: Ein skalerer Numeric, die Nummer der offenen Karte

# Ausgabe
# `numeric(1)`, entweder -1 oder 1.

# Beispiele
# ex04reward("equal", 2, 6)
#> [1] -1

# ex04reward("equal", 5, 5)
#> [1] 1

# ex04reward("larger", 5, 6)
#> [1] -1

# ex04reward("smaller", 5, 6)
#> [1] 1

# ex04reward("larger", -5, 6)
#> Error in ex04reward("larger", -5, 6) :
#>  Assertion on 'new_number' failed: Element 1 is not >= 1.

# ex04reward("larger", c(1, 1), 1)
#> Error in ex04reward("larger", c(1, 1), 1) :
#>  Assertion on 'new_number' failed: Must have length <= 1, but has length 2.

ex04reward <- function(decision, new_number, old_number) {
  if (!decision %in% c("larger", "smaller", "equal")) {
    stop("decision must be one of larger, smaller, equal")
  }
  checkmate::assert_integerish(new_number, lower = 1L, max.len = 1L)
  checkmate::assert_integerish(old_number, lower = 1L, max.len = 1L)
  if (decision == "larger") {
    if (new_number > old_number) {
      points <- 1
    } else {
      points <- -1
    }
  }
  #TODO
}
