BEGIN { OFS="" }

{
  print "ip netns exec ", $1, " wg set ", $1, " peer ", $2, " endpoint " $2, " "
}
