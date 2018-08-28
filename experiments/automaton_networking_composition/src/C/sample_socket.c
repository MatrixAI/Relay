/*
 * Sample program to figure out how raw ethernet sockets work.
 * We need to use raw ethernet sockets to mangle IP packets.
 *
 * - ramwan <ray.wan@matrix.ai>
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

#include <sys/socket.h>

#include <arpa/inet.h> // inet_ntop, inet_pton etc.

#include <netinet/udp.h>
#include <netinet/ip.h>
#include <netinet/ip6.h>
#include <netinet/if_ether.h> // wrapper around linux/if_ether.h

#include <net/ethernet.h> // ethernet constants and structures

int main(int argc, char **argv)
{
  // socket opened right above the device driver
  // sock_dgram so we don't have to worry about ethernet headers
  // ETH_P_IP defined in linux/if_ether.h for IP over ethernet protocol number
  //
  // man pages don't specify the use of htons() on the protocol, however the
  // majority of guides and answers to questions online use it.
  int sock_pack = socket(AF_PACKET, SOCK_DGRAM, htons(ETH_P_ALL));
  if (sock_pack < 0)
  {
    printf("Unable to create socket.\n");
    printf("%s\n", strerror(errno));
    return 0;
  }

  char buffer[2048];
  // https://stackoverflow.com/questions/16010622/reasoning-behind-c-sockets-sockaddr-and-sockaddr-storage#16010670
  struct sockaddr_storage saddr;
  socklen_t saddr_size;
  ssize_t recv_size;

  recv_size = recvfrom(sock_pack, (void *)&buffer, sizeof(buffer), 0, 
              (struct sockaddr *)&saddr, &saddr_size);

  printf("Got %ld bytes\n", recv_size);
  
  // print the packet we received in hex
  printf("PACKET [.......\n");
  for(int i=0; i<recv_size; i++)
  {
    printf("%x", buffer[i]);
  }
  printf("\n.......]\n");

  // check for ip version
  char ip_v = buffer[0] >> 4;
 
  if (ip_v == 4)
  {
    printf("Got an IPv4 packet.\n");

    struct iphdr *hdr = (struct iphdr *)buffer;
    char sender_addr[33];
    char recvr_addr[33]; // this is us
    
    inet_ntop(AF_INET, (void *)&hdr->saddr, sender_addr, sizeof(sender_addr));
    inet_ntop(AF_INET, (void *)&hdr->daddr, recvr_addr, sizeof(recvr_addr));
    sender_addr[32] = '\0';
    recvr_addr[32] = '\0';

    printf("Sent from %s\n", sender_addr);
    printf("Recieved on %s\n", recvr_addr);
  } else
  {
    printf("Got an IPv6 pacjet.\n");

    struct ip6_hdr *hdr = (struct ip6_hdr *)buffer;
    char snd_addr[129];
    char rcv_addr[129];

    inet_ntop(AF_INET6, (void *)&hdr->ip6_src, snd_addr, sizeof(snd_addr));
    inet_ntop(AF_INET6, (void *)&hdr->ip6_src, rcv_addr, sizeof(rcv_addr));
    snd_addr[128] = '\0';
    rcv_addr[128] = '\0';

    printf("Sent from %s\n", snd_addr);
    printf("Received on %s\n", rcv_addr);
  }

  close(sock_pack);
  return 0;
}
