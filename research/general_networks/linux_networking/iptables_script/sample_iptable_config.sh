# Sample IP table config
# by ramwan
#
# For this sample config, we will allow outgoing SSH connections to be
# established but incoming SSH attempts to be dropped. The current iptables
# config will be saved to $CONFIG_LOCATION.

CONFIG_LOCATION="/tmp/iptables_config_`date +%d:%m:%y::%H:%M`"

sudo iptables-save -f $CONFIG_LOCATION
echo "Current iptables config saved to $CONFIG_LOCATION"

sudo iptables -A INPUT -p tcp --dport 22 -m state --state \
NEW,UNTRACKED,INVALID -j DROP
