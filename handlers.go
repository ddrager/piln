package main

import (
	"encoding/json"
	"io/ioutil"
	"net/http"
	"strconv"

	"github.com/tidwall/gjson"
)

func orderCreate(w http.ResponseWriter, r *http.Request) {
	data, err := ioutil.ReadAll(r.Body)
	if err != nil {
		http.Error(w, "invalid json", 400)
		return
	}

	res := gjson.GetManyBytes(data, "cid", "note", "amount")
	cid := res[0].String()
	note := res[1].String()
	amount := res[2].Int()

	if amount < s.PriceGB {
		http.Error(w, "amount too low", 400)
		return
	}

	cid = toCID(cid)
	if cid == "" {
		http.Error(w, "wrong cid", 400)
		return
	}

	invoice, err := makeInvoice(cid, note, amount)
	if err != nil {
		log.Warn().Err(err).
			Str("cid", cid).Int64("amount", amount).Str("note", note).
			Msg("error making invoice")
		http.Error(w, "error making invoice, please contact us", 500)
		return
	}

	json.NewEncoder(w).Encode(invoice)
}

func orderStatus(w http.ResponseWriter, r *http.Request) {

}

func paymentCallback(w http.ResponseWriter, r *http.Request) {
	order_id := r.FormValue("order_id")
	price := r.FormValue("price")
	description := r.FormValue("description")
	id := r.FormValue("id")

	amount, err := strconv.Atoi(price)
	if err != nil {
		log.Warn().Err(err).Str("price", price).Str("id", id).
			Msg("got wrong 'price' from opennode callback")
		http.Error(w, "", 400)
	}

	if isInvoicePaid(id) {
		cid, note := splitDescription(description)
		err = savePayment(order_id, cid, amount, note)
		if err != nil {
			log.Error().Err(err).
				Str("cid", cid).
				Int("amount", amount).
				Msg("error saving payment")
		}

		go func() {
			err := processPayments()
			log.Error().Err(err).
				Msg("failed to process payments after getting a payment")
		}()

	} else {
		log.Warn().Err(err).Str("id", id).
			Msg("invoice reported as paid but not actually paid, why?")
		http.Error(w, "", 406)
	}

	w.WriteHeader(200)
}

func listObjects(w http.ResponseWriter, r *http.Request) {
	objs, err := fetchObjects()
	if err != nil {
		log.Error().Err(err).Msg("failed to fetch objects list")
		http.Error(w, "failed to fetch objects list", 500)
		return
	}

	json.NewEncoder(w).Encode(objs)
}

func getObject(w http.ResponseWriter, r *http.Request) {
	data, err := ioutil.ReadAll(r.Body)
	if err != nil {
		http.Error(w, "invalid json", 400)
		return
	}

	cid := gjson.GetBytes(data, "cid").String()
	obj, err := fetchObject(cid)
	if err != nil {
		http.Error(w, "object not found", 404)
		return
	}

	json.NewEncoder(w).Encode(obj)
}

func periodicJob(w http.ResponseWriter, r *http.Request) {
	go func() {
		err := processPayments()
		if err != nil {
			log.Error().Err(err).Msg("failed to process payments on periodic job")
			return
		}

		err = eraseEnded()
		if err != nil {
			log.Error().Err(err).Msg("failed to erase ended")
		}
	}()

	w.WriteHeader(200)
}
