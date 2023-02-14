
bnfc --haskell --outputdir=src -p Promela src/Promela.cf
alex src/Promela/LexPromela.x
happy src/Promela/ParPromela.y
rm src/Promela/TestPromela.hs