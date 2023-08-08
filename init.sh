# Parse grammar definitions
./parse-grammar.sh Promela
./parse-grammar.sh IR
./parse-grammar.sh Trace
stack build

# Install Dafny
./get-dafny.sh
