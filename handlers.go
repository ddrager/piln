package main

import (
	"encoding/json"
	"io/ioutil"
	"net/http"
	"strconv"

	"github.com/gorilla/mux"
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

	if len(note) > int(amount)*4 {
		http.Error(w, "note length should not be greater than amount paid * 4", 400)
		return
	}

	log.Info().Int64("amount", amount).Str("cid", cid).Str("note", note).
		Msg("payment order")

	cid = toCID(cid)
	if cid == "" {
		http.Error(w, "wrong cid", 400)
		return
	}

	invoice, order_id, err := makeInvoice(cid, note, amount)
	if err != nil {
		log.Warn().Err(err).
			Str("order_id", order_id).
			Str("cid", cid).Int64("amount", amount).Str("note", note).
			Msg("error making invoice")
		http.Error(w, "error making invoice, please contact us", 500)
		return
	}

	json.NewEncoder(w).Encode(struct {
		Invoice string `json:"invoice"`
		OrderId string `json:"order_id"`
	}{invoice, order_id})
}

func orderStatus(w http.ResponseWriter, r *http.Request) {
	order_id := mux.Vars(r)["orderId"]

	p, err := fetchPayment(order_id)
	if err != nil {
		log.Print(err)
		http.Error(w, "", 400)
		return
	}

	json.NewEncoder(w).Encode(p)
}

func paymentCallback(w http.ResponseWriter, r *http.Request) {
	order_id := r.FormValue("order_id")
	price := r.FormValue("price")
	description := r.FormValue("description")
	id := r.FormValue("id")

	log.Info().Str("oi", order_id).Str("p", price).Msg("payment callback")

	cid, note := splitDescription(description)

	amount, err := strconv.Atoi(price)
	if err != nil {
		log.Warn().Err(err).Str("price", price).Str("id", id).
			Msg("got wrong 'price' from opennode callback")
		http.Error(w, "", 400)
	}

	if isInvoicePaid(id) {
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
		return
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
	log.Info().Msg("periodic job")

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
