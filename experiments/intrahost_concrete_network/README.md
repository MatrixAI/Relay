This experiment shows that wireguard isn't as flexible as we thought it was.

17:49:07.800674 IP 0.0.0.0.51820 > 10.1.0.1.51821: UDP, length 148
17:49:12.920640 IP 0.0.0.0.51820 > 10.1.0.1.51821: UDP, length 148
17:49:18.552767 IP 0.0.0.0.51820 > 10.1.0.1.51821: UDP, length 148
17:49:21.624456 IP localhost.51822 > localhost.51820: UDP, length 32
17:49:43.640638 IP 0.0.0.0.51820 > 10.1.0.1.51821: UDP, length 148
17:49:46.712513 IP localhost.51822 > localhost.51820: UDP, length 32
  17:49:46.712697 IP localhost.51822 > localhost.51820: UDP, length 148
  17:49:46.713148 IP localhost.51820 > localhost.51822: UDP, length 92
  17:49:46.713401 IP localhost.51822 > localhost.51820: UDP, length 32
17:49:48.760671 IP 0.0.0.0.51820 > 10.1.0.1.51821: UDP, length 148
17:49:54.392632 IP 0.0.0.0.51820 > 10.1.0.1.51821: UDP, length 148

That snippet shows 148byte key re-establishment messages and hwo it occurs at
the re-establishment of an already established communication context. However
0.0.0.0.51820 > 10.1.0.1.51821 is a wireguard server talking to a wireguard
client and the server, rather than sending a normal handshake keeps sending a
re-establishment and the server expects the client to re-create the keypair as
in the wireguard whitepaper it says

"This time-based opportunistic rekeying is restricted to the initiator of the
current session."

but the server is the current session so the client cannot restart the handshake
and so the server is stuck in this dodgy position.



Furthermore, in logs/ I ran a bunch of `iperf` tests which compares a simple
veth setup using veth pairs and linux bridges to the wireguard setup. Even
though the wireguard setup won't be actually used, I was still interested in
seeing how the performance of the 2 compared. For both UDP and TCP connections,
veth pairs performed better by far. So much better in fact that even if
wireguard didn't have the issue related with server/client connection states, it
would be worth considering replacing wireguard with plain veth connections.
However do note that the bitrate for wireguard links for UDP connections was
actually the slightest bit higher for some reason but the latency was
significantly higher than a veth connection.
