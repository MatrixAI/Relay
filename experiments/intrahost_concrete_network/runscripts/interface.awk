BEGIN { OFS="" }

{
  print "s/ route add ", $1," / route add ", $2, "\\/64 /"
  print "s/ ", $1, " / ", $2, " /"
}
