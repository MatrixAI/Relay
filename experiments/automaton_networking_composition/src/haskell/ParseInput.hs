module ParseInput
  where

-- COMPOSITION
import Composition1
--

import qualified System.IO as IO
import Definitions

--
returnComposition :: Composition
returnComposition = composition

--
instantiateNetwork :: Composition
instantiateNetwork c = automatons c

--
-- https://stackoverflow.com/questions/10730838/how-to-create-multiple-network-namespace-from-a-single-process-instance
-- ns_path = "/var/run/netns/" ++ ns_name
-- close(open(ns_path, O_RDONLY|O_CREAT|O_EXCL, 0));
createNetns :: Name -> IO ()
createNetns n = 
        let ns_path = "/var/run/netns/" ++ n
        in IO.openFile ns_path IO.ReadWriteMode
           >>= hClose

--
-- https://unix.stackexchange.com/questions/241173/how-are-dev-linux-files-created#241200
--
-- Examination of strace output for creating a veth pair
--   o s = socket(AF_NETLINK, SOCK_RAW|SOCK_CLOEXEC, NETLINK_ROUTE)
--   o sockaddr_nl = {nl_family=AF_NETLINK, nl_pad=0, nl_pid=0, nl_groups=0}
--   o bind(s, sockaddr_nl, sizeof(sockaddr_nl))
--   o
--   o nlmsghdr = {len, type=RTM_NEWLINK, flags=NLM_F_REQUEST|NLM_F_ACK, seq=0,
--                 pid=0}
--   o ifinfomsg = {ifi_family=AF_UNSPEC, ifi_type=ARPHRD_NETROM, ifi_index=0,
--                  ifi_flags=0, ifi_changes=0}
--   o msg = [nlmsghdr, ifinfomsg]
--   o sendto(s, msg, len, 0, NULL, 0)
--   o received back an error of ENODEV ....?
--   o
--   o 
createVethPair ::
createVethPair = 

