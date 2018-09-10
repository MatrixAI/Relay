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

/*
 * HELPER FUCNTIONS
 */

//we should check for ENOMEM here
void nlmsg_put_attr(struct nlmsghdr *nlmsg, int attr, const void *data, size_t len)
{
  struct rtattr *rt = NLMSG_TAIL(nlmsg);
  size_t rta_len = RTA_LENGTH(len);
  size_t t_len = NLMSG_ALIGN(nlmsg->nlmsg_len) + RTA_ALIGN(rta_len);

  rt->rta_type = attr;
  rt->rta_len = rta_len;

  if(data && len)
    memcpy(RTA_DATA(rt), data, len);

  nlmsg->nlmsg_len = t_len;
}

//
struct rtattr *nlmsg_nest(struct nlmsghdr *nlmsg, int attr)
{
  struct rtattr *rt = NLMSG_TAIL(nlmsg);
  nlmsg_put_attr(nlmsg, attr, NULL, 0);

  return rt;
}

//
void nlmsg_end_nest(struct nlmsghdr *nlmsg, struct rtattr *rta)
{
  rta->rta_len = (void *)NLMSG_TAIL(nlmsg) - (void *)rta;
}



/*
 * MAIN
 */

int main(int argc, char **argv)
{
  int res;
  int sock = socket(AF_NETLINK, SOCK_RAW|SOCK_CLOEXEC, NETLINK_ROUTE);
  int snd_buf[32768]; int rcv_buf[1048576]; int ack_buf;

  res = setsockopt(sock, SOL_SOCKET, SO_SNDBUF, snd_buf, 4);
  if(res != 0)
  {
    printf("error with setsockopt(sock, SOL_SOCKET, SO_SNDBUF, snd_buf, 4)\n");
    printf("%d => %s\n", errno, strerror(errno));
    return 0;
  }
  res = setsockopt(sock, SOL_SOCKET, SO_RCVBUF, rcv_buf, 4);
  if(res != 0)
  {
    printf("error with setsockopt(sock, SOL_SOCKET, SO_RCVBUF, rcv_buf, 4)\n");
    printf("%d => %s\n", errno, strerror(errno));
    return 0;
  }
  res = setsockopt(sock, SOL_NETLINK, NETLINK_EXT_ACK, &ack_buf, 4);
  if(res != 0)
  {
    printf("error with setsockopt(sock, SOL_NETLINK, NETLINK_EXT_ACK, ack_buf, 4)\n");
    printf("%d => %s\n", errno, strerror(errno));
    return 0;
  }

  struct sockaddr_nl addr;
  memset(&addr, 0, sizeof(addr));
  addr.nl_family = AF_NETLINK;

  res = bind(sock, (struct sockaddr *)&addr, sizeof(addr));
  
  if(res < 0)
  {
    printf("unable to bind: %s", strerror(errno));
    return 0;
  }

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

  struct msghdr msg_hdr;
  struct nlmsghdr nl_hdr; struct ifinfomsg ifmsg;
  struct iovec iov; struct req *rq;

  memset(&ifmsg, 0, sizeof(ifmsg));
  ifmsg.ifi_family = AF_UNSPEC;
  ifmsg.ifi_type = ARPHRD_NETROM;

  nl_hdr.nlmsg_len = NLMSG_LENGTH(sizeof(struct ifinfomsg));
  nl_hdr.nlmsg_type = RTM_NEWLINK;
  nl_hdr.nlmsg_flags = NLM_F_REQUEST|NLM_F_ACK|NLM_F_EXCL|NLM_F_CREATE;
  nl_hdr.nlmsg_seq = 0;
  nl_hdr.nlmsg_pid = 0;

  msg_hdr.msg_name = (void *)&addr;
  msg_hdr.msg_namelen = sizeof(addr);
  msg_hdr.msg_iov = &iov;
  msg_hdr.msg_iovlen = 1;
  msg_hdr.msg_control = NULL;
  msg_hdr.msg_controllen = 0;
  msg_hdr.msg_flags = 0;

  rq = (struct req *)&nl_hdr;
  rq->ifm = ifmsg;

  // Create the message
  struct rtattr *r1 = nlmsg_nest(&nl_hdr, IFLA_LINKINFO);
  nlmsg_put_attr(&nl_hdr, IFLA_INFO_KIND, "veth", 4);
  struct rtattr *r2 = nlmsg_nest(&nl_hdr, IFLA_INFO_DATA);
  struct rtattr *r3 = nlmsg_nest(&nl_hdr, VETH_INFO_PEER);
  
  nl_hdr.nlmsg_len += sizeof(struct ifinfomsg); //idk why...

  nlmsg_put_attr(&nl_hdr, IFLA_IFNAME, "v2", 2);
  nlmsg_end_nest(&nl_hdr, r3);
  nlmsg_end_nest(&nl_hdr, r2);
  nlmsg_end_nest(&nl_hdr, r1);

  nlmsg_put_attr(&nl_hdr, IFLA_IFNAME, "v1", 2);

  iov.iov_base = (void *)&rq;
  iov.iov_len = nl_hdr.nlmsg_len;

  //printf("msg_hdr.msg_name.nl_family = %d\n", msg_hdr.msg_name.nl_family);
  //printf("AF_NETLINK = %d\n", AF_NETLINK);

  // Send the message
  res = sendto(sock, &msg_hdr, sizeof(msg_hdr), 0, NULL, 0);
  
  if(res < 0)
  {
    printf("error with sendmsg(sock, &msg_hdr, 0)\n");
    printf("%d => %s", errno, strerror(errno));
    return 0;
  }
  else
  {
    printf("sendmsg(sock, &msg_hdr, 0) = %d\n", res);
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
  msg_hdr.msg_name = &addr;
  msg_hdr.msg_namelen = sizeof(addr);
  msg_hdr.msg_iovlen = 1;
  iov.iov_base = malloc(1024);
  iov.iov_len = 1024;
  res = recvmsg(sock, &msg_hdr, 0);
  
  if(res<0)
  {
    printf("error with recvmsg(sock, &msg_hdr, 0)\n");
    printf("%d => %s", errno, strerror(errno));
  }
  else
  {
    printf("recvmsg(sock, &msg_hdr, 0) = %d\n", res);
    struct nlmsghdr *nlhdr = iov.iov_base;
    struct nlmsgerr *err = iov.iov_base + sizeof(struct nlmsghdr);
    printf("nlhdr->len = %d\n", nlhdr->nlmsg_len);
    printf("err->error = %d\n", err->error);
    printf("err->msg->type = %d\n", err->msg.nlmsg_type);
    printf("RTM_NEWLINK = %d\n", RTM_NEWLINK);
    printf("iov_len = %ld\n", iov.iov_len);
  }
  free(iov.iov_base);

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
