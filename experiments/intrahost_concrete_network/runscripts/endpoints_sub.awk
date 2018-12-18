BEGIN { OFS="" }

{
  print "s/ip netns exec ", $1, " /ip netns exec ", $4, " /"
  print "s/ peer ", $1, " / peer ", $5, " /"
  print "s/ endpoint ", $1, "/ endpoint [", $2, "]:", $3, "/"
}
