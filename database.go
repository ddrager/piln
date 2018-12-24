package main

import (
	"database/sql"
	"time"

	"github.com/lib/pq"
)

type Object struct {
	CID      string         `json:"cid" db:"cid"`
	SizeGB   float64        `json:"sizegb" db:"sizegb"`
	PinnedAt time.Time      `json:"pinned_at" db:"pinned_at"`
	EndsAt   time.Time      `json:"ends_at" db:"ends_at"`
	Notes    pq.StringArray `json:"notes" db:"notes"`
}

type Payment struct {
	OrderId   string   `json:"order_id" db:"order_id"`
	CID       string   `json:"cid" db:"cid"`
	Note      string   `json:"note" db:"note"`
	PaidAt    string   `json:"paid_at" db:"paid_at"`
	Amount    int      `json:"amount" db:"amount"`
	Status    string   `json:"status" db:"status"`
	Tries     int      `json:"tries" db:"tries"`
	Recycling []string `json:"recycling" db:"recycling"`
}

func fetchObjects() (oo []Object, err error) {
	oo = make([]Object, 0)
	err = pg.Select(&oo, `
SELECT cid, sizegb, pinned_at, pinned_at + lifespan AS ends_at, notes(o)
FROM objects AS o
WHERE pinned_at + lifespan > now()
ORDER BY ends_at ASC
    `)
	return
}

func fetchObject(cid string) (*Object, error) {
	o := Object{}
	err = pg.Get(&o, `
SELECT cid, sizegb, pinned_at, pinned_at + lifespan AS ends_at, notes(o)
FROM objects AS o WHERE cid = $1
    `, cid)
	if err == sql.ErrNoRows {
		return nil, nil
	}
	return &o, err
}

func fetchPayment(orderId string) (*Payment, error) {
	p := Payment{}
	err = pg.Get(&p, `
SELECT order_id, cid, note, paid_at, amount, status, tries
FROM payments AS o WHERE order_id = $1
    `, orderId)
	if err == sql.ErrNoRows {
		return nil, nil
	}
	return &p, err
}

func savePayment(order_id, cid string, paidAmount int, note string, orders []string) error {
	_, err := pg.Exec(`
WITH reused_orders AS (
  UPDATE payments
  SET status = 'repurposed'
  WHERE status = 'given_up'
  AND order_id = any($1)
  RETURNING order_id, amount
)
INSERT INTO payments (order_id, cid, note, amount, recycling)
VALUES (
  $3,
  $4,
  $5,
  (SELECT coalesce(sum(amount), 0) + $2 FROM reused_orders),
  (SELECT coalesce(array_agg(order_id), '{}'::text[]) FROM reused_orders)
)
    `, pq.Array(orders), paidAmount, order_id, cid, note)
	return err
}
