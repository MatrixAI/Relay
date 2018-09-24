module Composition1
  where

import Definitions


{-
 - 2 automatons composed in a "->" manner.
 - One instance of one automaton
 - and two instances of the other.
 -}
composition :: Composition
composition =
        let instance_a1 = Instance {
                netnsName = "netns_a1",
                vethNames = [
                  hashAndHex "veth1_a1",
                  hashAndHex "veth2_a1"
                          ],
                defaultGateway = concreteAddress "fc00::1"
                       }
            instance_b1 = Instance {
                netnsName = "netns_b1",
                vethNames = [
                  hashAndHex "veth1_b1",
                  hashAndHex "veth2_b1"
                            ],
                defaultGateway = concreteAddress "fc00::2"
                       }
            instance_b2 = Instance {
                netnsName = "netns_b2",
                vethNames = [
                  hashAndHex "veth1_b2",
                  hashAndHex "veth2_b2"
                            ],
                defaultGateway = concreteAddress "fc00::3"
                       }
            a = Automaton {
                  name = "a",
                  numberInstances = 1,
                  dependencyFlows = [],
                  compositionFlows = [compose (flowID "fd00::1") b],
                  instances = [instance_a1]
                          }
            b = Automaton {
                  name = "b",
                  numberInstances = 2,
                  dependencyFlows = [],
                  compositionFlows = [],
                  instances = [instance_b1, instance_b2]
                          }
            l_a = [a, b]
        in Composition {
                         automatons = l_a,
                         flowTable = createFlowTable l_a
                       }
            
