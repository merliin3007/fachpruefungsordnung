# FPO-Frontend

Dieses Projekt bildet das Frontend der Anwendung ab, geschrieben in PureScript mit dem Halogen Package als Basis.

## Setup

### Voraussetzungen
- ``npm`` (bestenfalls version 10.9.x)
- ``node`` (bestenfalls version v22.15.x)
- ``spago`` (erhält man via `npm install -g spago@next`)
- ``esbuild`` (erhält man via `npm install -g esbuild`)
- ``purs`` (erhält man via `npm install -g purescript`)

All dies ist benötigt, um das Projekt vollständig bauen zu können.

### Frontend starten 

Hier ist die komplette Einrichtung von Grund auf beschrieben. Wenn du Schritt 1 und 2 einmal gemacht hast, braucht man nur noch Schritt 3 und 4 (solange keine neuen Packages dazukommen).

1. Rufe ``npm install`` auf. Damit sollten die benötigten externen Module heruntergeladen werden.
2. Rufe ``spago install`` auf. Damit sollten alle benötigten purescript-Module heruntergeladen werden.
3. Rufe ``spago build`` auf. Damit sollte eine Datei `index.js` erstellt werden.
4. Öffne die Datei ``index.html`` im Browser deiner Wahl und erstaune das Endprodukt

## Halogen 
Dies ist das Frontend-Framework, was dem Code zu Grunde liegt. Für eine gute Dokumentation schaue [diesen Guide](https://purescript-halogen.github.io/purescript-halogen/) an.
