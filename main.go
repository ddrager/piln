package main

import (
	"net/http"
	"os"
	"time"

	"github.com/dghubble/sling"
	"github.com/gorilla/mux"
	shell "github.com/ipfs/go-ipfs-api"
	"github.com/jmoiron/sqlx"
	"github.com/kelseyhightower/envconfig"
	_ "github.com/lib/pq"
	"github.com/rs/zerolog"
)

type Settings struct {
	ServiceName     string  `envconfig:"SERVICE_NAME" required:"true"`
	ServiceURL      string  `envconfig:"SERVICE_URL" required:"true"`
	Port            string  `envconfig:"PORT" required:"true"`
	PostgresURL     string  `envconfig:"DATABASE_URL" required:"true"`
	OpenNodeURL     string  `envconfig:"OPENNODE_URL" required:"true"`
	OpenNodeKey     string  `envconfig:"OPENNODE_KEY" required:"true"`
	IPFSAPIURL      string  `envconfig:"IPFS_API_URL" required:"true"`
	AbsoluteMaxSize float64 `envconfig:"ABSOLUTE_MAX_SIZE" required:"true"`
	PriceGB         int64   `envconfig:"PRICE_GB" required:"true"`
}

var err error
var s Settings
var r *mux.Router
var pg *sqlx.DB
var on *sling.Sling
var ipfs *shell.Shell
var log = zerolog.New(os.Stderr).Output(zerolog.ConsoleWriter{Out: os.Stderr})

func main() {
	err = envconfig.Process("", &s)
	if err != nil {
		log.Fatal().Err(err).Msg("couldn't process envconfig.")
	}

	zerolog.SetGlobalLevel(zerolog.DebugLevel)
	log = log.With().Timestamp().Logger()

	// opennode helper
	on = sling.New().
		Base(s.OpenNodeURL).
		Set("Content-Type", "application/json").
		Set("Authorization", s.OpenNodeKey)

	// ipfs helper
	ipfs = shell.NewShell(s.IPFSAPIURL)

	// postgres connection
	pg, err = sqlx.Connect("postgres", s.PostgresURL)
	if err != nil {
		log.Fatal().Err(err).Msg("couldn't connect to postgres")
	}

	// define routes
	r = mux.NewRouter()
	r.Path("/favicon.ico").Methods("GET").HandlerFunc(
		func(w http.ResponseWriter, r *http.Request) {
			http.ServeFile(w, r, "./public/icon.png")
			return
		})
	r.Path("/api/order").Methods("POST").HandlerFunc(orderCreate)
	r.Path("/api/order/{orderId}").Methods("GET").HandlerFunc(orderStatus)
	r.Path("/api/objects").Methods("GET").HandlerFunc(listObjects)
	r.Path("/api/object/{cid}").Methods("GET").HandlerFunc(getObject)
	r.Path("/callback/order").Methods("POST").HandlerFunc(paymentCallback)
	r.Path("/cron/periodic").Methods("POST").HandlerFunc(periodicJob)
	r.PathPrefix("/").Methods("GET").Handler(http.FileServer(http.Dir("./static")))

	// start the server
	srv := &http.Server{
		Handler:      r,
		Addr:         "0.0.0.0:" + s.Port,
		WriteTimeout: 25 * time.Second,
		ReadTimeout:  25 * time.Second,
	}
	log.Info().Str("port", s.Port).Msg("listening.")
	srv.ListenAndServe()
}
