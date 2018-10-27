package main

import (
	"errors"
	"strings"
)

func toCID(cid string) string {
	if strings.Contains(cid, "/ipfs/") {
		cid = strings.Split(cid, "/ipfs/")[1]
	}
	return strings.TrimSpace(cid)
}

func pin(cid string, max float64) (sizegb float64, err error) {
	stats, err := ipfs.ObjectStat(cid)
	if err != nil {
		return
	}

	sizegb = float64(stats.CumulativeSize) / 1000000000
	if sizegb > max || sizegb > s.AbsoluteMaxSize {
		err = errors.New("object too big")
		return
	}

	err = ipfs.Pin(cid)
	return
}

func unpin(cid string) error {
	return ipfs.Unpin(cid)
}
