module ParseInput
  where

-- COMPOSITION
--import Composition1
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

-- see create_veth.c in src/C/veth_creation
createVethPair ::
createVethPair = 

