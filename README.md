# haskell

Dieses Ordner beinhaltet Aufgaben und Lösungen in der Sprache Haskell, die ich während dem Studium gelernt habe, zum Teil auch mit Erklärungen.

STEPS : 
- Haskell-Compiler herunterladenund installieren.Wählen Sie dabei „stack“ aus.
 https://www.haskell.org/downloads/

- VS-Code starten.

- „Terminal“ im VS öffnen und die folgenden Befehle tippen:
	a. Ein Haskell-Projekt (z.B. Aufgaben) erstellen:
	       stack new Aufgaben
	b. Den Ordner wechseln:
	       cd Aufgaben
	c. Die notwendigen Dateien automatisch erstellen lassen:
	       stack build

- Linke Seite des Fensters, unter dem Ordner „src“ eine Haskell-Datei anlegen (z.B. Aufgabe02.hs)

- Schreiben Sie Ihr Programm (z.B):
	module Aufgabe02 where
	add x y = x + y
(*Benutzen Sie keine Einrückungen (z.B. durch Tab-Taste).
Alle Programmzeilen müssen unter der ersten Spalte der Seite geschrieben werden.)

- Speichern Sie die Datei.

-Den Haskell-Compiler starten:
	stack ghci

- Rufen Sie eine Funktion aus Ihrem Programm auf (z.B. die Funktion add):
	add 2 3
  Das Ergebnis wird in dem Terminal angezeigt. 

- Nach jeder Änderung des Programms können Sie das Programm nochmals reloaden:
 	:r