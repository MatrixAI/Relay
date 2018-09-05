module Composition1
  where

import qualified Data.IP as IP
import qualified Data.Hashmap.Strict as Map
import Definitions

-- A B
-- 1 1
-- A -> B
composition :: 
composition = 
        let a = Automaton {
                  name="a",
                  numberInstances=1,
                  dependencyFlows=[],
                  compositionFlows=[read "fd00::1" :: FlowID]
                  instances=[instance_a1]
                          }
            b = Automaton {
                  name="b",
                  numberInstances=1,
                  dependencyFlows=[],
                  compositionFlows=[]
                  instances=[instance_b1, instance_b2]
                          }
            instance_a1 = Instance {
                            netnsName=name a,
                            vethNames=[
                              hashToHex $ name a,
                              hashToHex $ show a
                                      ],
                            defaultGateway=read "fc00::1" :: IP.IPv6
                                   }
            instance_b1 = Instance {
                            netnsName=name b ++ "1",
                            vethNames=[
                              hashToHex $ name b ++ "1",
                              hashToHex $ show b ++ "1"
                                      ],
                              defaultGateway=read "fc00::2" :: IP.IPv6
                                   }
            instance_b2 = Instance {
                            netnsName=name b ++ "2",
                            vethNames=[
                              hashToHex $ name b ++ "2",
                              hashToHex $ show b ++ "2",
                                      ],
                              defaultGateway=read "fc00::3" :: IP.IPv6
                                   }
            rule_a2b = Rule {
                         toFlowId=read "fd00:1" :: FlowID,
                         toAutomaton=b
                            }
            translation_table = singleton 

