setup()    { rm -rf foobar; }
teardown() { rm -rf foobar; ghc-pkg unregister foobar &>/dev/null; }

test_sqlite()     { ../test/scaffold.sh < ../test/sqlite-input.txt    ; }
test_postgresql() { ../test/scaffold.sh < ../test/postgresql-input.txt; }
#test_mongodb()    { ../test/scaffold.sh < ../test/mongodb-input.txt   ; }
test_tiny()       { ../test/scaffold.sh < ../test/tiny-input.txt      ; }
