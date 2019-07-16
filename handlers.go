package main

import (
     "encoding/json"
     "io/ioutil"
     "net/http"
     "strconv"
     "fmt"
     "strings"

     "github.com/gorilla/mux"
     "github.com/lucsky/cuid"
     "github.com/tidwall/gjson"
)

func getGlobals(w http.ResponseWriter, r *http.Request) {
     globals := struct {
          PriceGB       int64    `json:"priceGB"`
          IPFSID        string   `json:"ipfsID"`
          IPFSAddresses []string `json:"ipfsAddresses"`
     }{s.PriceGB, "temporarily offline", make([]string, 0)}

     info, err := ipfs.ID()
     if err != nil {
          log.Error().Err(err).Msg("ipfs node is offline?")
     } else {
          globals.IPFSID = info.ID
          globals.IPFSAddresses = filterOutLocal(info.Addresses)
     }

     json.NewEncoder(w).Encode(globals)
}

func orderCreate(w http.ResponseWriter, r *http.Request) {
     data, err := ioutil.ReadAll(r.Body)
     if err != nil {
          http.Error(w, "invalid json", 400)
          return
     }

     log.Info().Msg("message: in orderCreate")

     res := gjson.GetManyBytes(data, "cid", "note", "amount", "reused_orders")
     cid := res[0].String()
     note := res[1].String()
     amount := res[2].Int()
     reusedOrders := res[3]

     orders := make([]string, len(reusedOrders.Array()))
     var i = 0
     reusedOrders.ForEach(func(_, value gjson.Result) bool {
          orders[i] = value.String()
          i++
          return true
     })

     if amount == 0 && len(orders) == 0 {
          http.Error(w, "cannot pay zero", 400)
          return
     }

     if len(note) > 23 && len(note) > int(amount) {
          http.Error(w, "note length should not be greater than 23 or amount paid", 400)
          return
     }

     log.Info().Int64("amount", amount).Str("cid", cid).Str("note", note).
          Msg("payment order")

     cid = toCID(cid)
     if cid == "" {
          http.Error(w, "wrong cid", 400)
          return
     }

     var order_id string
     var invoice string
     if amount == 0 {
          // end the order here, don't generate an invoice
          order_id = cuid.New()
          err = savePayment(order_id, cid, 0, note, orders)
          if err != nil {
               log.Error().Err(err).
                    Str("cid", cid).
                    Msg("error saving payment with just reused orders.")
          }

          go func() {
               err := processPayments()
               log.Error().Err(err).
                    Msg("failed to process payments after getting pure-reuse order")
          }()
     } else {
          // the process will continue on the webhook we'll get from opennode
          invoice, order_id, err = makeInvoice(cid, note, amount, orders)
          if err != nil {
               log.Warn().Err(err).
                    Str("order_id", order_id).
                    Str("cid", cid).Int64("amount", amount).Str("note", note).
                    Msg("error making invoice")
               http.Error(w, "error making invoice, please contact us", 500)
               return
          }
     }

     json.NewEncoder(w).Encode(struct {
          Invoice string `json:"invoice,omitempty"`
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

// formatRequest generates ascii representation of a request
func formatRequest(r *http.Request) string {
     // Create return string
     var request []string
     // Add the request string
     url := fmt.Sprintf("%v %v %v", r.Method, r.URL, r.Proto)
     request = append(request, url)
     // Add the host
     request = append(request, fmt.Sprintf("Host: %v", r.Host))
     // Loop through headers
     for name, headers := range r.Header {
          name = strings.ToLower(name)
          for _, h := range headers {
               request = append(request, fmt.Sprintf("%v: %v", name, h))
          }
     }
     // If this is a POST, add post data
     if r.Method == "POST" {
          r.ParseForm()
          request = append(request, "\n")
          request = append(request, r.Form.Encode())
     }
     // Return the request as a string
     return strings.Join(request, "\n")
}

func paymentCallback(w http.ResponseWriter, r *http.Request) {
     log.Info().Msg("Callback was received, in handler")
     request_pretty := formatRequest(r)
     order_id := r.FormValue("order_id")
     price := r.FormValue("price")
     description := r.FormValue("description")
     id := r.FormValue("id")

     log.Info().Str("oi", order_id).Str("p", price).Str("req",request_pretty).Msg("payment callback")
     log.Info().Str("description",description).Msg("paymentCallback")

     cid, reusedOrders, note := splitDescription(description)

     paidAmount, err := strconv.Atoi(price)
     if err != nil {
          log.Warn().Err(err).Str("price", price).Str("id", id).
               Msg("got wrong 'price' from opennode callback")
          http.Error(w, "", 400)
     }

     if isInvoicePaid(id) {
          err = savePayment(order_id, cid, paidAmount, note, reusedOrders)
          if err != nil {
               log.Error().Err(err).
                    Str("cid", cid).
                    Int("amount", paidAmount).
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
     log.Info().Msg("Getting objects")
     objs, err := fetchObjects()
     if err != nil {
          log.Error().Err(err).Msg("failed to fetch objects list")
          http.Error(w, "failed to fetch objects list", 500)
          return
     }

     json.NewEncoder(w).Encode(objs)
}

func getObject(w http.ResponseWriter, r *http.Request) {
     log.Info().Msg("Getting object")
     data, err := ioutil.ReadAll(r.Body)
     if err != nil {
          http.Error(w, "invalid json", 400)
          return
     }

     cid := gjson.GetBytes(data, "cid").String()
     obj, err := fetchObject(cid)
     if err != nil {
	  log.Error().Err(err).
	                         Msg("Not found error")
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
