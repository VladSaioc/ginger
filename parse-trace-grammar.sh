
bnfc --haskell --outputdir=src -p Trace src/Trace.cf
alex src/Trace/LexTrace.x
happy src/Trace/ParTrace.y
rm src/Trace/TestTrace.hs