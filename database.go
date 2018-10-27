package main

import "time"

type Object struct {
	CID      string    `json:"cid"`
	SizeGB   int       `json:"sizegb"`
	PinnedAt time.Time `json:"pinned_at"`
	EndsAt   time.Time `json:"ends_at"`
	Notes    []string  `json:"notes"`
}

func fetchObjects() (oo []Object, err error) {
	err = pg.Select(&oo, `
SELECT cid, sizegb, pinned_at, pinned_at + lifespan AS ends_at
FROM objects
    `)
	return
}

func fetchObject(cid string) (o Object, err error) {
	err = pg.Get(&o, `
SELECT cid, sizegb, pinned_at, pinned_at + lifespan AS ends_at
FROM objects WHERE cid = $1
    `, cid)
	return
}

func savePayment(order_id, cid string, amount int, note string) error {
	_, err := pg.Exec(`
INSERT INTO payments (order_id, cid, amount, note)
VALUES ($1, $2, $3, $4)
    `, order_id, cid, amount, note)
	return err
}

func processPayments() error {
	txn, err := pg.Beginx()
	if err != nil {
		return err
	}

	var payments []struct {
		CID    string `db:"cid"`
		Amount int64  `db:"amount"`
	}
	err = txn.Select(&payments, `
SELECT cid, amount FROM payments WHERE NOT processed
    `)
	if err != nil {
		txn.Rollback()
		return err
	}

	for _, payment := range payments {
		sizegb, err := pin(
			payment.CID,
			float64(payment.Amount)/float64(s.PriceGB),
		)
		if err != nil {
			txn.Rollback()
			return err
		}

		duration := time.Hour * time.Duration(
			float64(payment.Amount)/float64(s.PriceGB/24)/sizegb,
		)

		_, err = txn.Exec(`
INSERT INTO objects (cid, sizegb, pinned_at, lifespan)
VALUES ($1, $2, $3, $4)
ON CONFLICT (cid) DO UPDATE SET pinned_at = $3, lifespan = lifespan + $4
        `, payment.CID, sizegb, time.Now(), duration)
	}

	return txn.Commit()
}

func eraseEnded() error {
	var cids []string
	err := pg.Select(&cids, `
SELECT cid FROM objects WHERE pinned_at + lifespan < now()
    `)
	if err != nil {
		return err
	}

	for _, cid := range cids {
		err = unpin(cid)
		if err != nil {
			return err
		}

		_, err = pg.Exec(`
DELETE FROM objects WHERE cid = $1
        `, cid)
		if err != nil {
			return err
		}
	}

	return nil
}
