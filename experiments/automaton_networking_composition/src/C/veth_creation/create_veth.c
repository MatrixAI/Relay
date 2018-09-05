#include <stdio.h>
#include <stdlib.h>
#include <asm/types.h>
#include <sys/socket.h>
#include <linux/netlink.h>
#include <linux/rtnetlink.h>

int main(int argc, char **argv)
{
  int sock = socket(AF_NETLINK, SOCK_RAW|SOCK_CLOEXEC, NETLINK_ROUTE);

  /*
sendmsg(3,
        { msg_name=nl,
          msg_namelen=12,
          msg_iov=[
            { iov_base=
              { nlmsg,
                ifmsg, 
                [ {nattr1, "v1"},
                  {nattr2,
                  [ { nattr3, "veth"...},
                    { nattr4,
                      "\x1c\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x07\x00\x03\x00\x76\x32\x00\x00"
                    }
                  ]
                  }
                ]
              }, iov_len=84
            }], msg_iovlen=1, msg_controllen=0, msg_flags=0}, 0) = 84

    struct sockaddr_nl nl = {sa_family=AF_NETLINK, nl_pid=0, nl_groups=0};
    struct nlmsghdr nlmsg = {len=84, type=RTM_NEWLINK, flags=NLM_F_REQUEST|NLM_F_ACK|NLM_F_EXCL|NLM_F_CREATE, seq=*seq num*, pid=0}
    struct ifinfomsg ifmsg = {ifi_family=AF_UNSPEC, ifi_type=ARPHRD_NETROM, ifi_index=0, ifi_flags=0, ifi_change=0}
    struct nlattr nattr1 = {nla_len=7, nla_type=IFLA_IFNAME}
    struct nlattr nattr2 = {nla_len=44, nla_type=IFLA_LINKINFO}
    struct nlattr nattr3 = {nla_len=8, nla_type=IFLA_INFO_KIND}
    struct nlattr nattr4 = {nla_len=32, nla_type=IFLA_INFO_DATA}
   */

  return 0;
}
