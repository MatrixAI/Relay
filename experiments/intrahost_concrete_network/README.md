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
