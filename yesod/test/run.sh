#!/bin/bash -e
#
# Runs test/scaffold.sh with a variety of inputs. Hides all output
# besides failure details.
#
###

[[ "$1" =~ -v|--verbose ]] && stdout=/dev/stdout || stdout=/dev/null

tmp='/tmp'
pwd="$PWD"

pkg=
dir=

failures=()
n_tested=0
n_failed=0

# runs the function named by $1, silencing stdout and redirecting stderr
# to /tmp/function.errors. failures are tracked to be reported on during
# cleanup
run_test() { # {{{
  local test_function="$*"

  n_tested=$((n_tested+1))

  if $test_function >"$stdout" 2>"$tmp/$test_function.errors"; then
    echo -n '.'
    [[ -f "$tmp/$test_function.errors" ]] && rm "$tmp/$test_function.errors"
  else
    echo -n 'F'
    failures+=( "$test_function" )
    n_failed=$((n_failed+1))
  fi
}
# }}}

# changes back to the original directory, removes the dist file and
# outputs a report of tests and failures
cleanup() { # {{{
  cd "$pwd"
  [[ -d "$dir" ]] && rm -r "$dir"

  echo
  echo
  echo "Tests: $n_tested, Failures: $n_failed."
  echo

  [[ $n_failed -eq 0 ]] && return 0

  for test in ${failures[@]}; do
    echo "Failure: $test"
    echo 'details:'
    echo

    if [[ -f "$tmp/$test.errors" ]]; then
      cat "$tmp/$test.errors"
      rm  "$tmp/$test.errors"
    else
      echo '<no stderr captured>'
    fi

    echo
  done

  return $n_failed
}
# }}}

# compilation is test #1, sets global variable dir. other tests are run
# from within this directory and it is removed as part of cleanup
test_compile() {
  cabal clean
  cabal install
  cabal sdist

  read -r pkg < <(find dist/ -type f -name '*.tar.gz' | sort -rV)
  dir="$(basename "$pkg" .tar.gz)"

  tar -xzf "$pkg" && cd "$dir"
}

test_sqlite()     { ../test/scaffold.sh < ../test/sqlite-input.txt    ; }
test_postgresql() { ../test/scaffold.sh < ../test/postgresql-input.txt; }
test_mongodb()    { ../test/scaffold.sh < ../test/mongodb-input.txt   ; }
test_tiny()       { ../test/scaffold.sh < ../test/tiny-input.txt      ; }

echo 'Started'
run_test 'test_compile'
run_test 'test_sqlite'
run_test 'test_postgresql'
run_test 'test_mongodb'
run_test 'test_tiny'
cleanup

exit $?
