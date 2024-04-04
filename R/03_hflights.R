# Frage 3 (30P)

# Betrachten Sie den Datensatz "hflights" aus dem Paket `hflights`.
library(hflights)
library(tidyverse)
data("hflights")
summary(hflights)
# Es ist Ihre Aufgabe den Datensatz gemäß der Vorgaben in den Teilaufgaben zu 
# analysieren.
# Sie können Ihren Ansatz mit jeweiligen Tests überprüfen.
# Da Sie aber keine Funktionen schreiben, sind die Tests eher binär.
# Die Tests werden grün, wenn Sie die Aufgabe so gelöst haben, wie die 
# Musterlösung es erwartet.
# Für die Tests ist es wichtig, dass Sie die jeweiligen Objekte korrekt bezeichnen.
# Für reverse engineerte Tests ohne Code erhalten Sie Null Punkte.
# Die Tests sind immer gleich aufgebaut: Zunächst wird überprüft, ob Ihre Lösung 
# das richtige Format hat (z.B data.frame mit 10 Zeilen und 3 Spalten inkl. 
# Spaltennamen). Dann werden Statistiken Ihres Objektes (z.B. Median einer 
# speziellen Spalte) getestet.
# Im Umkehrschluss bedeutet das, dass, wenn erst die Assertions bzgl. der 
# Statistiken Fehler produzieren, die Format Checks bereits durchgelaufen sind.

# a)
# Wandeln Sie alle Spalten der Klasse `character` in eine Spalte der Klasse 
# `factor` um.
# Speichern Sie das Ergebnis in "ex0302a". Arbeiten Sie anschließend mit diesem 
# Datensatz weiter.
# Für diese Teilaufgabe steht Ihnen der Test "3.1" zur Verfügung.
ex0302a <- NULL

# b)
# Filtern Sie nach allen Flügen, die annulliert ("cancelled") wurden.
# Welche Fluggesellschaft hat die meisten Annullierungen?
# Beantworten Sie diese Frage mit einem `data.frame`, das für jeden `UniqueCarrier` 
# (Spalte 1) eine Anzahl `n` (Spalte 2) der Annullierungen enthält und sortieren 
# Sie es in absteigender Reihenfolge nach `n`.
# Speichern Sie das Resultat als `ex0302b`.
# Für diese Teilaufgabe steht Ihnen der Test "3.2" zur Verfügung.
ex0302b <- NULL

# c)
# Anstelle von absoluten Werten interessiert uns ein Annulierungs-Ratio pro Carrier.
# Berechnen Sie diesen Ratio und geben Sie ihn in einem `data.frame` mit zwei Spalten 
# (`UniqueCarrier` und `CancellationRatio`) aus.
# Sortieren Sie ihn so, dass die höchsten Ratios zuerst erscheinen.
# Runden Sie auf 4 Ziffern.
# Speichern Sie das Resultat als `ex0302c`.
# Für diese Teilaufgabe steht Ihnen der Test "3.3" zur Verfügung.
ex0302c <- NULL

# d)
# Als zweite Kennzahl interessiert uns die durchschnittliche Verzögerung (`MeanDelay`)
# bei der Ankunft (`ArrDelay`).
# Berechnen Sie diese Kennzahl  pro Fluggesselschaft (`Unique Carrier`).
# Achten Sie darauf, dass bei der Berechnung der Verspätung (`ArrDelay`), nur
# positive Werte für die Berechnung verwendet werden
# (eine negative Verspätung ist mit einer frühen Ankunft verbunden),
# grundsätzlich aber alle Daten verwendet werden.
# Runden Sie die Werte wie zuvor auf 4 Ziffern.
# Speichern Sie das resultierende `data.frame` mit zwei Spalten als `ex0302d`.
# Für diese Teilaufgabe steht Ihnen der Test "3.4" zur Verfügung.
ex0302d <- NULL

# e)
# Ermitteln Sie die Anzahl der Flüge, die mit Verspätung gestartet sind (`DepDelay`),
# aber ohne Verspätung landen konnten (`ArrDelay`).
# Speichern Sie das Resultat als `ex0302e`.
# Für diese Teilaufgabe steht Ihnen der Test "3.5" zur Verfügung.
ex0302e <- NULL
