BEGIN { OFS="" }

{
  print "s/ route add ", $1," / route add ", $2, "\\/64 /"
  print "s/address add ", $1, " /address add ", $2, " /"
  print "s/ ", $1, " / ", $2, " /"
}
