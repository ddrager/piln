package main

import (
	"strings"

	"github.com/c2h5oh/datasize"
)

func toCID(cid string) string {
	if strings.Contains(cid, "/ipfs/") {
		cid = strings.Split(cid, "/ipfs/")[1]
	}
	return strings.TrimSpace(cid)
}

func size(cid string) (float64, error) {
	stats, err := ipfs.ObjectStat(cid)
	if err != nil {
		return 0, err
	}

	size := datasize.ByteSize(stats.CumulativeSize).GBytes()
	return size, nil
}

func pin(cid string) error {
	return ipfs.Pin(cid)
}

func unpin(cid string) error {
	return ipfs.Unpin(cid)
}
