BEGIN { OFS="" }

{
  print "ip -n $HOSTNET -6 route add ", $1, " dev lo"
  print "ip -n $HOSTNET -6 route add ", $2, " dev lo"
  print "ip netns exec $HOSTNET ip6tables -t nat -A PREROUTING -p udp -d "\
    , $3, " -s ", $1, " -j DNAT --to-destination ", $2, " "
  print "ip netns exec $HOSTNET ip6tables -t nat -A POSTROUTING -p udp -s "\
    , $1, " -d ", $2, " -j SNAT --to-source ", $3, " "
  print "ip netns exec $HOSTNET ip6tables -t nat -A PREROUTING -p udp -d "\
    , $3, " -s ", $2, " -j DNAT --to-destination ", $1, " "
  print "ip netns exec $HOSTNET ip6tables -t nat -A POSTROUTING -p udp -s "\
    , $2, " -d ", $1, " -j SNAT --to-source ", $3, " "
}
