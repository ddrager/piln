package main

import (
	"errors"
	"io/ioutil"
	"net/http"
	"strings"

	"github.com/lucsky/cuid"
	"github.com/tidwall/gjson"
)

const SEPARATOR = "|||"

func splitDescription(desc string) (string, string) {
	s := strings.SplitN(desc, SEPARATOR, 2)
	return s[0], s[1]
}

func isInvoicePaid(id string) bool {
	req, _ := on.Get("/v1/charge/" + id).Request()
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		log.Warn().Str("id", id).Err(err).Msg("failed to fetch charge")
		return false
	}

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Warn().Str("id", id).Err(err).Msg("got wrong response from /charge")
		return false
	}

	return "paid" == gjson.GetBytes(body, "data.status").String()
}

func makeInvoice(cid string, note string, amount int64) (string, error) {
	description := cid + SEPARATOR + note
	callback_url := s.ServiceURL + "/callback/order"
	order_id := cuid.New()

	req, _ := on.Post("/v1/charges").BodyJSON(struct {
		Description string `json:"description"`
		Amount      int64  `json:"amount"`
		OrderId     string `json:"order_id"`
		CallbackURL string `json:"callback_url"`
	}{description, amount, order_id, callback_url}).Request()

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return "", err
	}

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}

	success := gjson.GetBytes(body, "success")
	if success.Exists() && success.Bool() == false {
		log.Error().Str("err", gjson.GetBytes(body, "message").String()).
			Msg("failed to make invoice")
		return "", errors.New("failed to make invoice")
	}

	invoice := gjson.GetBytes(body, "data.lightning_invoice.payreq").String()
	return invoice, nil
}
