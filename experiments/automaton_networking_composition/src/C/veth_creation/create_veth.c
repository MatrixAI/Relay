/*
 * If this code is a bit confusing, there is a reference project found at
 * https://github.com/inokappa/lxc/blob/f49ca85ccd2c089a4d1d7d9f208fd4a3cf4410ab/src/lxc/network.c
 * - lxc
 *
 * ramwan <ray.wan@matrix.ai>
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <asm/types.h>
#include <sys/socket.h>
#include <linux/netlink.h>
#include <linux/rtnetlink.h>
#include <linux/if_arp.h>
#include <linux/veth.h>
#include <unistd.h>

// helper macro to create a struct rtattr at the end of the given netlink
// message
#define NLMSG_TAIL(msg) ((struct rtattr *)(((void *)(msg)) + NLMSG_ALIGN(msg->nlmsg_len)))

struct req {
  struct nlmsghdr nh;
  struct ifinfomsg ifm;
};

//
struct rtattr *nlmsg_recurse(struct nlmsghdr *nlmsg, int attr, const void *data, size_t len)
{
  struct rtattr *rt = NLMSG_TAIL(nlmsg);
  rt->rta_len = RTA_LENGTH(len);
  rt->rta_type = attr;
  if(len > 0) memcpy(RTA_DATA(rt), data, len);

  nlmsg->nlmsg_len = NLMSG_ALIGN(nlmsg->nlmsg_len) + RTA_ALIGN(rt->rta_len);

  return rt;
}

//
void nlmsg_close_recurse(struct nlmsghdr *nlmsg, struct rtattr *rt)
{
  rt->rta_len = (void *)NLMSG_TAIL(nlmsg) - (void *)rt;
}


int main(int argc, char **argv)
{
  int sock = socket(AF_NETLINK, SOCK_RAW|SOCK_CLOEXEC, NETLINK_ROUTE);
  char snd_buf[32768]; char rcv_buf[1048576]; char ack_buf[1];

  setsockopt(sock, SOL_SOCKET, SO_SNDBUF, snd_buf, 4);
  setsockopt(sock, SOL_SOCKET, SO_RCVBUF, rcv_buf, 4);
  setsockopt(sock, SOL_NETLINK, NETLINK_EXT_ACK, ack_buf, 4);

  struct sockaddr_nl addr;
  addr.nl_family = AF_NETLINK;
  addr.nl_pid = 0;
  addr.nl_groups = 0;
  bind(sock, (struct sockaddr *)&addr, sizeof(struct sockaddr));

  /*
    struct nlmsghdr hdr = {len=32, type=RTM_NEWLINK, flags=NLM_F_REQUEST|NLM_F_ACK, seq=0, pid=0}
    struct ifinfomsg ifmsg = {ifi_family=AF_UNSPEC, ifi_type=ARPHRD_NETROM, ifi_index=0, ifi_flags=0, ifi_change=0}

    sendto(3, { hdr, ifmsg }, 32, 0, NULL, 0) = 32
   */
  struct nlmsghdr nl_hdr; struct ifinfomsg ifmsg;
  int msg_size = sizeof(struct nlmsghdr) + sizeof(struct ifinfomsg);
  
  ifmsg.ifi_family = AF_UNSPEC;
  ifmsg.ifi_type = ARPHRD_NETROM;
  ifmsg.ifi_index = 0;
  ifmsg.ifi_flags = 0;
  ifmsg.ifi_change = 0;
  nl_hdr.nlmsg_len = NLMSG_LENGTH(msg_size);
  nl_hdr.nlmsg_type = RTM_NEWLINK;
  nl_hdr.nlmsg_flags = NLM_F_REQUEST|NLM_F_ACK;
  nl_hdr.nlmsg_seq = 11;
  nl_hdr.nlmsg_pid = 0;

  // could also be done by creating a new struct type
  char buf[msg_size];
  memcpy(&buf[0], &nl_hdr, sizeof(struct nlmsghdr));
  memcpy(&buf[sizeof(struct nlmsghdr)], &ifmsg, sizeof(struct ifinfomsg));

  int res = sendto(sock, (void *)&buf, msg_size, 0, NULL, 0);
  printf("sendto(sock, (void *)&buf, %d, 0, NULL, 0) = %d\n", msg_size, res);

  if(res < 0)
  {
    printf("error -> %s\n", strerror(errno));
    return 0;
  }
  else
  {
    printf("sent %d bytes\n", res);
  }

  /*
    recvmsg(3, hdr, 0)

    struct msghdr hdr =
            {
              msg_name={sa_family=AF_NETLINK, nl_pid=0, nl_groups=00000000}, // struct sockaddr
              msg_namelen= sizeof(struct sockaddr),
              msg_iov=[
                        {
                          iov_base={
                                     {len=52, type=NLMSG_ERROR, flags=0, seq=0, pid=747},
                                     { error=-ENODEV, 
                                       msg={
                                             {len=32, type=RTM_NEWLINK, flags=NLM_F_REQUEST|NLM_F_ACK, seq=0, pid=0},
                                             {ifi_family=AF_UNSPEC, ifi_type=ARPHRD_NETROM, ifi_index=0, ifi_flags=0, ifi_change=0}
                                           }
                                     }
                                   },
                          iov_len=16384
                        }
                      ],
              msg_iovlen=1, 
              msg_controllen=0,
              msg_flags=0
            }
   */
  struct msghdr msg_hdr;
  struct iovec iov[1];
  iov[0].iov_base = malloc(1024);
  iov[0].iov_len = 1024;

  msg_hdr.msg_name = (struct sockaddr *)&addr;
  msg_hdr.msg_namelen = sizeof(struct sockaddr);
  msg_hdr.msg_iov = iov;
  msg_hdr.msg_iovlen = 1;

  sleep(1);
  printf("trying to recv message\n");
  res = 0;//recvmsg(sock, &msg_hdr, 0);

  printf("recvmsg(sock, &msg_hdr, 0) = %d\n", res);
  if(res < 0)
  {
    printf("error -> %s\n", strerror(errno));
    return 0;
  }
  else
  {
    // NLMSG_OK() to check if message is ok.... but skipped here
    printf("yea it's ok...\n");
  }
  free(iov[0].iov_base);

  /*
    sendmsg(3, hdr, 0)

    struct sockaddr_nl nl = {sa_family=AF_NETLINK, nl_pid=0, nl_groups=0};
    struct nlmsghdr nlmsg = {len=84, type=RTM_NEWLINK, flags=NLM_F_REQUEST|NLM_F_ACK|NLM_F_EXCL|NLM_F_CREATE, seq=*seq num*, pid=0}
    struct ifinfomsg ifmsg = {ifi_family=AF_UNSPEC, ifi_type=ARPHRD_NETROM, ifi_index=0, ifi_flags=0, ifi_change=0}
    struct nlattr nattr1 = {nla_len=7, nla_type=IFLA_IFNAME}
    struct nlattr nattr2 = {nla_len=44, nla_type=IFLA_LINKINFO}
    struct nlattr nattr3 = {nla_len=8, nla_type=IFLA_INFO_KIND}
    struct nlattr nattr4 = {nla_len=32, nla_type=IFLA_INFO_DATA}

    struct msghdr hdr =
        { msg_name=nl,
          msg_namelen=12,
          msg_iov=[
            { iov_base=
              { nlmsg,
                ifmsg, 
                [ 
                  {nattr1, "v1"},
                  {nattr2,
                  [
                    { nattr3, "veth"...},
                    { nattr4,
                      "\x1c\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x07\x00\x03\x00\x76\x32\x00\x00"
                    }
                  ]
                  }
                ]
              },
              iov_len=84
            }],
          msg_iovlen=1,
          msg_controllen=0,
          msg_flags=0}
   */
  struct req rq;
  rq.nh = nl_hdr;
  rq.ifm = ifmsg;
  
  struct rtattr *r1 = nlmsg_recurse(&nl_hdr, IFLA_LINKINFO, NULL, 0);
  struct rtattr *r2 = nlmsg_recurse(&nl_hdr, IFLA_INFO_KIND, "veth", 5);
  struct rtattr *r3 = nlmsg_recurse(&nl_hdr, IFLA_INFO_DATA, NULL, 0);
  struct rtattr *r4 = nlmsg_recurse(&nl_hdr, VETH_INFO_PEER, NULL, 0);
  struct rtattr *r5 = nlmsg_recurse(&nl_hdr, IFLA_IFNAME, "v2", 3);

  nlmsg_close_recurse(&nl_hdr, r1);
  nlmsg_close_recurse(&nl_hdr, r2);
  nlmsg_close_recurse(&nl_hdr, r3);
  nlmsg_close_recurse(&nl_hdr, r4);
  nlmsg_close_recurse(&nl_hdr, r5);

  msg_hdr.msg_name = (struct sockaddr *)&addr;
  msg_hdr.msg_namelen = sizeof(struct sockaddr);
  msg_hdr.msg_iovlen = 1;
  msg_hdr.msg_controllen = 0;
  msg_hdr.msg_flags = 0;
  msg_hdr.msg_iov = iov;

  iov[0].iov_base = (void *)&rq;
  iov[0].iov_len = nl_hdr.nlmsg_len + 1;

  res = sendmsg(sock, &msg_hdr, 0);
  
  if(res < 0)
  {
    printf("error with second sendto(sock, &msg_hdr, 0)\n");
    printf("%s\n", strerror(errno));
    return 0;
  }
  else
  {
    printf("sendto(sock, &msg_hdr, 0) = %d\n", res);
  }

  /*
    recvmsg(3,
            {
              msg_name={sa_family=AF_NETLINK, nl_pid=0, nl_groups=00000000},
              msg_namelen=12,
              msg_iov=[{iov_base=NULL, iov_len=0}],
              msg_iovlen=1,
              msg_controllen=0,
              msg_flags=MSG_TRUNC
            },
            MSG_PEEK|MSG_TRUNC)
   */
  msg_hdr.msg_name = (struct sockaddr *)&addr;
  msg_hdr.msg_namelen = sizeof(struct sockaddr);
  msg_hdr.msg_iovlen = 1;
  iov[0].iov_base = malloc(1024);
  iov[0].iov_len = 1024;
  res = recvmsg(sock, &msg_hdr, 0);
  
  if(res<0)
  {
    printf("error: %s\n", strerror(errno));
  }
  else
  {
    printf("recvmsg(sock, &msg_hdr, 0) = %d\n", res);
    struct nlmsghdr *nlhdr = iov[0].iov_base;
    struct nlmsgerr *err = iov[0].iov_base + sizeof(struct nlmsghdr);
    printf("nlhdr->len = %d\n", nlhdr->nlmsg_len);
    printf("err->error = %d\n", err->error);
    printf("err->msg->type = %d\n", err->msg.nlmsg_type);
    printf("RTM_NEWLINK = %d\n", RTM_NEWLINK);
    printf("iov_len = %ld\n", iov[0].iov_len);
  }
  free(iov[0].iov_base);

  /*
    recvmsg(3,
            {
              msg_name={sa_family=AF_NETLINK, nl_pid=0, nl_groups=00000000},
              msg_namelen=12,
              msg_iov=[
                        {
                          iov_base={
                                     {len=36, type=NLMSG_ERROR, flags=NLM_F_CAPPED, seq=1536286166, pid=747},
                                     {error=0,
                                     msg={len=84, type=RTM_NEWLINK, flags=NLM_F_REQUEST|NLM_F_ACK|NLM_F_EXCL|NLM_F_CREATE, seq=1536286166, pid=0}
                                     }
                                   },
                          iov_len=36
                        }
                      ],
              msg_iovlen=1,
              msg_controllen=0,
              msg_flags=0
            },
            0)
   */

  return 0;
}
