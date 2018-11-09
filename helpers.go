package main

import "strings"

func filterOutLocal(multiaddresses []string) []string {
	addresses := make([]string, 0, len(multiaddresses))
	for _, addr := range multiaddresses {
		if strings.HasPrefix(addr, "/ip4/10.") ||
			strings.HasPrefix(addr, "/ip4/192.") ||
			strings.HasPrefix(addr, "/ip4/172.") ||
			strings.HasPrefix(addr, "/ip4/0.") ||
			strings.HasPrefix(addr, "/ip6/:") ||
			strings.HasPrefix(addr, "/ip4/127.") {
			continue
		}

		addresses = append(addresses, addr)
	}

	return addresses
}
