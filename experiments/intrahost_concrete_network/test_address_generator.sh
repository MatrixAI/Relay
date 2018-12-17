# run_test(address_type host port automaton
run_test () {
  python3 address_generator.py "$1" "$2" "$3" "$4"
}

# check($? val test_item test_val
check() {
  if [[ "$1" != "$2" ]]; then
    echo "$3 accepted: $4"
  fi
}

# test ports
test_ports() {
port=0
run_test "real" 0 $port 0 >/dev/null
check $? 1 "port" $port
port=1024
run_test "real" 0 $port 0 >/dev/null
check $? 1 "port" $port
port=1025
run_test "real" 0 $port 0 >/dev/null
check $? 0 "port" $port
port=8080
run_test "real" 0 $port 0 >/dev/null
check $? 0 "port" $port
port=65535
run_test "real" 0 $port 0 >/dev/null
check $? 0 "port" $port
port=65537
run_test "real" 0 $port 0 >/dev/null
check $? 1 "port" $port
port=0
run_test "flow" 0 $port 0 >/dev/null
check $? 1 "port" $port
port=1024
run_test "flow" 0 $port 0 >/dev/null
check $? 1 "port" $port
port=1025
run_test "flow" 0 $port 0 >/dev/null
check $? 0 "port" $port
port=8080
run_test "flow" 0 $port 0 >/dev/null
check $? 0 "port" $port
port=65535
run_test "flow" 0 $port 0 >/dev/null
check $? 0 "port" $port
port=65537
run_test "flow" 0 $port 0 >/dev/null
check $? 1 "port" $port
}

test_types() {
  type="real"
  run_test $type 0 8080 0 >/dev/null
  check $? 0 "type" $type
  type="flow"
  run_test $type 0 8080 0 >/dev/null
  check $? 0 "type" $type
  type="asdfasfdgr"
  run_test $type 0 8080 0 >/dev/null
  check $? 1 "type" $type
  type="fl0w"
  run_test $type 0 8080 0 >/dev/null
  check $? 1 "type" $type
}

test_addresses() {
  addr=`run_test "real" "ffffffffff" 65535 "ffffffffffffffff"`
  check "$addr" "fcff:ffff:ffff:ffff:ffff:ffff:ffff:ffff" "address" "$addr"
  addr=`run_test "real" 0 8080 0`
  check "$addr" "fc00:0:0:1f90::" "address" "$addr"
  addr=`run_test "real" "ffffffffffffff" 65535 "ffffffffffffffffffffffff"`
  check "$addr" "fcff:ffff:ffff:ffff:ffff:ffff:ffff:ffff" "address" "$addr"
  addr=`run_test "flow" "ffffffffff" 65535 "ffffffffffffffff"`
  check "$addr" "fdff:ffff:ffff:ffff:ffff:ffff:ffff:ffff" "address" "$addr"
  addr=`run_test "flow" 0 8080 0`
  check "$addr" "fd00:0:0:1f90::" "address" "$addr"
  addr=`run_test "flow" "ffffffffffffff" 65535 "ffffffffffffffffffffffff"`
  check "$addr" "fdff:ffff:ffff:ffff:ffff:ffff:ffff:ffff" "address" "$addr"
}


test_types
test_ports
test_addresses
