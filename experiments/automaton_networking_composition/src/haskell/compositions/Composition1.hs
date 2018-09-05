module Composition1
  where

import Definitions

instance_a1 = Instance {
                netnsName = "netns_a1",
                vethNames = [
                  hashToHex "veth1_a1",
                  hashToHex "veth2_a1"
                          ],
                defaultGateway = concreteAddress "fc00::1"
                       }
instance_b1 = Instance {
                netnsName = "netns_b1",
                vethNames = [
                  hashToHex "veth1_b1",
                  hashToHex "veth2_b1"
                            ],
                defaultGateway = concreteAddress "fc00::2"
                       }
instance_b2 = Instance {
                netnsName = "netns_b2",
                vethNames = [
                  hashToHex "veth1_b2",
                  hashToHex "veth2_b2"
                            ],
                defaultGateway = concreteAddress "fc00::3"
                       }
{-
 - 2 automatons composed in a "->" manner.
 - One instance of one automaton
 - and two instances of the other.
 -}
composition :: Composition
composition =
        let a = Automaton {
                  name = "a",
                  numberInstances = 1,
                  dependencyFlows = [],
                  compositionFlows = [(flowID "fd00::1", b)],
                  instances = [instance_a1]
                          }
            b = Automaton {
                  name = "b",
                  numberInstances = 1,
                  dependencyFlows = [],
                  compositionFlows = [],
                  instances = [instance_b1, instance_b2]
                          }
            flow_table = foldl flowTableIns newFlowTable $ compositionFlows a
        
        in Composition{automatons=[
                                    a,
                                    b
                                  ],
                       flowTable=flow_table
                      }
            
