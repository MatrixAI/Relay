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
#include <arpa/inet.h>

// helper macro to create a struct rtattr at the end of the given netlink
// message
#define NLMSG_TAIL(msg) ((struct rtattr *)(((void *)(msg)) + NLMSG_ALIGN(msg->nlmsg_len)))

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
  char snd_buf[32768]; char rcv_buf[1048576]; char ack_buf;

  res = setsockopt(sock, SOL_SOCKET, SO_SNDBUF, snd_buf, 4);
  res = setsockopt(sock, SOL_SOCKET, SO_RCVBUF, rcv_buf, 4);
  res = setsockopt(sock, SOL_NETLINK, NETLINK_EXT_ACK, &ack_buf, 4);

  struct sockaddr_nl addr;
  memset(&addr, 0, sizeof(addr));
  addr.nl_family = AF_NETLINK;
  addr.nl_pid = getpid();

  res = bind(sock, (struct sockaddr *)&addr, sizeof(addr));
  
  if(res != 0)
  {
    printf("unable to bind: %s", strerror(errno));
    return 0;
  }




  struct sockaddr_nl sender_addr;
  struct nlmsghdr *nl_hdr = malloc(1024);
  struct ifinfomsg ifmsg; struct iovec iov;
  
  memset(&ifmsg, 0, sizeof(struct ifinfomsg));
  ifmsg.ifi_family = AF_UNSPEC;
  ifmsg.ifi_type = ARPHRD_NETROM;
  ifmsg.ifi_change = 0xFFFFFFFF;

  nl_hdr->nlmsg_len = NLMSG_LENGTH(sizeof(struct ifinfomsg));
  nl_hdr->nlmsg_type = RTM_NEWLINK;
  nl_hdr->nlmsg_flags = NLM_F_REQUEST|NLM_F_CREATE|NLM_F_EXCL|NLM_F_ACK;
  nl_hdr->nlmsg_seq = 0;
  nl_hdr->nlmsg_pid = 0;

  // Create the message
  struct rtattr *r1 = nlmsg_nest(nl_hdr, IFLA_LINKINFO);
  nlmsg_put_attr(nl_hdr, IFLA_INFO_KIND, "veth", 5);
  struct rtattr *r2 = nlmsg_nest(nl_hdr, IFLA_INFO_DATA);
  struct rtattr *r3 = nlmsg_nest(nl_hdr, VETH_INFO_PEER);
  
  nl_hdr->nlmsg_len += sizeof(struct ifinfomsg);

  nlmsg_put_attr(nl_hdr, IFLA_IFNAME, "v2", 3);

  nlmsg_end_nest(nl_hdr, r3);
  nlmsg_end_nest(nl_hdr, r2);
  nlmsg_end_nest(nl_hdr, r1);

  nlmsg_put_attr(nl_hdr, IFLA_IFNAME, "v1", 3);

// print
for(int i=0; i<nl_hdr->nlmsg_len; i++)
{
  printf("%x ", *((char *)nl_hdr + i*sizeof(char)));
}
printf("\n");

  struct msghdr msg_hdr = {
    .msg_name = &sender_addr,
    .msg_namelen = sizeof(sender_addr),
    .msg_iov = &iov,
    .msg_iovlen = 1,
    .msg_control = NULL,
    .msg_controllen = 0,
    .msg_flags = 0
  };

printf("Stats: \n");
printf("IFLA_LINKINFO = %x\n", IFLA_LINKINFO);
printf("IFLA_INFO_KIND = %x\n", IFLA_INFO_KIND);
printf("IFLA_INFO_DATA = %x\n", IFLA_INFO_DATA);
printf("VETH_INFO_PEER = %x\n", VETH_INFO_PEER);

// print
for(int i=0; i<nl_hdr->nlmsg_len; i++)
{
  printf("%x ", *((char *)nl_hdr + i*sizeof(char)));
}
printf("\n");

  memset(&sender_addr, 0, sizeof(sender_addr));
  sender_addr.nl_family = AF_NETLINK;

  iov.iov_len = nl_hdr->nlmsg_len;
  iov.iov_base = malloc(nl_hdr->nlmsg_len);
  memcpy(iov.iov_base, nl_hdr, nl_hdr->nlmsg_len - sizeof(struct ifinfomsg));
  memcpy(iov.iov_base + nl_hdr->nlmsg_len - sizeof(struct ifinfomsg),
         &ifmsg,
         sizeof(struct ifinfomsg));

  // send the msg
  res = sendmsg(sock, &msg_hdr, 0);
  
  if(res < 0)
  {
    printf("error with sendmsg(sock, &msg_hdr, 0) = %d\n", res);
    printf("%d => %s", errno, strerror(errno));
    return 0;
  }
  else
  {
    printf("sendmsg(sock, &msg_hdr, 0) = %d\n", res);
  }

  


  struct sockaddr_nl recv_addr;
  memset(&recv_addr, 0, sizeof(struct sockaddr_nl));
  recv_addr.nl_family = AF_NETLINK;

  struct msghdr recv_msg_hdr = {
    .msg_name = &recv_addr,
    .msg_namelen = sizeof(struct sockaddr_nl),
    .msg_iov = &iov,
    .msg_iovlen = 1,
    .msg_control = NULL,
    .msg_controllen = 0,
    .msg_flags = 0
  };
  free(iov.iov_base);
  iov.iov_base = malloc(1024);

  res = recvmsg(sock, &recv_msg_hdr, 0);
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
    
    printf("err->error = %d\n", err->error);
    printf("errno %d => %s\n", -err->error, strerror(-err->error));
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
