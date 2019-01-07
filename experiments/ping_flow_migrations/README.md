# Ping Flow Migration experiment

The very first experiment I did can be found at
../stateless_container_ping_migrations. That experiment attempted to show the
possibilities of using abstract addresses when sending ICMP ping packets between
network namespaces (simulating the network namespaces of automatons).
Unfortunatly (or fortunately) in that experiment things didn't quite work out
but this current simple experiment aims to reproduce the previous setup in a
much simple environment and with more knowledge on my part.

## Run the experiment

The bash script `run.sh` pretty much does everything. Run the script with sudo
permissions and you will be thrown into an input loop. From here if you enter
"swap", the script will run commands that alternate iptables mappings.

Once you have started `run.sh`, you need to open up another terminal window.
From there, run `ip netns exec A ping fe00::1`. Go back to the terminal window
with `run.sh` and enter swap. Notice how the `ping` program never reports an
error. You can verify that the ICMP packets are indeed going to different
network namespaces upon telling `run.sh` to swap by running in more separate
terminals `ip netns exec C tcpdump` and `ip netns exec D tcpdump`.

## Notes

Naturally curious, you should wonder whether you can decrease the interval of
`ping` and have no error messages be reported. I have tried down to `ping -i
0.00001` and have yet to see an error message.

Instead of running ping, you can run `ncat` servers in netns C and netns D. For
example, `ip netns exec C ncat -u -l fc01::1 5555` and `ip netns exec D ncat -u
-l fc02::2 5555`. And then `ip netns exec A ncat -u fe00::1 5555`. From there
you can send udp packets from client to server.

Now what about tcp instead of udp? Well ncat isn't so happy with tcp in this
situation. The client (netns A) gets terminated upon telling `run.sh` to swap
the destination namespaces which is expected as TCP is a connection oriented
protocol.

Another thing to note is that the ordering of iptables update commands and
conntrack delete commands is important. Iptables has to be updated first before
the conntrack entry is deleted.
