package main

import (
	"database/sql"
	"errors"
	"fmt"
	"strings"
	"time"
)

func processPayments() error {
	pg.Exec(`UPDATE payments SET status = 'given_up' WHERE tries > 20`)

	payments := make([]struct {
		OrderId string `db:"order_id"`
		CID     string `db:"cid"`
		Amount  int64  `db:"amount"`
	}, 0)
	err = pg.Select(&payments, `
UPDATE payments
SET tries = payments.tries + 1
WHERE status = 'trying'
RETURNING order_id, cid, amount
    `)
	if err != nil && err != sql.ErrNoRows {
		return err
	}

	log.Debug().Int("n", len(payments)).Msg("processing payments")
	if len(payments) == 0 {
		return nil
	}

	jobs := make(chan error, len(payments))
	for _, payment := range payments {
		go func(orderId string, cid string, amount int64) {
			logger := log.With().
				Str("order_id", orderId).
				Int64("amount", amount).
				Str("cid", cid).Logger()

			var err error
			var sizegb float64

			logger.Debug().Msg("processing payment")

			if o, err := fetchObject(cid); err == nil && o != nil {
				logger.Info().Msg("object already pinned. no need to pin again.")
				sizegb = o.SizeGB
				goto savingOnDatabase
			}

			sizegb, err = size(cid)
			if err != nil {
				jobs <- err
				logger.Error().Err(err).Msg("failed to get size")
				return
			}

			logger = logger.With().Float64("sizegb", sizegb).Logger()

			if sizegb > float64(amount)/float64(s.PriceGB) {
				err = fmt.Errorf("object too big for the payment: %v > %v / %v",
					sizegb, amount, s.PriceGB)
			} else if sizegb > s.AbsoluteMaxSize {
				err = fmt.Errorf("object absolutely too big: %v > %v",
					sizegb, s.AbsoluteMaxSize)
			}
			if err != nil {
				logger.Error().Err(err).Msg("")
				jobs <- err
				return
			}

			err = pin(cid)
			if err != nil {
				logger.Error().Err(err).Msg("pin failed")
				jobs <- err
				return
			}
			logger.Info().Float64("sizegb", sizegb).
				Msg("pinned")

		savingOnDatabase:
			duration := time.Hour * time.Duration(
				float64(amount)/float64(s.PriceGB/24)/sizegb,
			)

			_, err = pg.Exec(`
WITH c AS (
  UPDATE payments
  SET status = 'pinned'
  WHERE order_id = $1
)
INSERT INTO objects (cid, sizegb, pinned_at, lifespan)
VALUES ($2, $3, now(), make_interval(secs := $4))
ON CONFLICT (cid)
  DO UPDATE SET lifespan = objects.lifespan + make_interval(secs := $4)
            `, orderId, cid, sizegb, duration.Seconds())
			jobs <- err
			return
		}(payment.OrderId, payment.CID, payment.Amount)
	}

	allfinished := make(chan bool, 1)
	go func() {
		for _ = range jobs {

		}
		allfinished <- true
	}()

	select {
	case _ = <-allfinished:
		return nil
	case <-time.After(60 * time.Minute):
		return errors.New("timeout")
	}
}

func eraseEnded() error {
	var cids []string
	err := pg.Select(&cids, `
SELECT cid FROM objects WHERE pinned_at + lifespan < now()
    `)
	if err != nil {
		return err
	}

	log.Debug().Str("cids", strings.Join(cids, ",")).Msg("erasing ended")
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
