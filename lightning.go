package main

import (
	"io/ioutil"
	"net/http"
	"strings"

	"github.com/lucsky/cuid"
	"github.com/tidwall/gjson"
)

const SEPARATOR = "|^~|"

func splitDescription(desc string) (string, string) {
	s := strings.SplitN(desc, SEPARATOR, 1)
	return s[0], s[1]
}

func isInvoicePaid(id string) bool {
	req, _ := on.Get("/charge/" + id).Request()
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

	req, _ := on.Post("/charges").BodyJSON(struct {
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

	invoice := gjson.GetBytes(body, "data.lightning_invoice.payreq").String()
	return invoice, nil
}
