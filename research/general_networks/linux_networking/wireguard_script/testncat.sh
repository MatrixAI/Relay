PORT=$1
exec 3<>/dev/udp/10.2.0.1/${PORT}
echo "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" >&3
