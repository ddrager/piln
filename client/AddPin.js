/** @format */

const fetch = window.fetch
const fromNow = require('fromnow')
const prettyBytes = require('pretty-bytes')

import React, {useState, useEffect, useContext} from 'react' // eslint-disable-line no-unused-vars
import {QRCode} from 'react-qr-svg'
import {toast} from 'react-toastify'

import {GlobalContext} from './Main'

export default function AddPin({cid: selectedCid = '', onAfterPaid}) {
  let {priceGB} = useContext(GlobalContext)

  let [o, setObject] = useState({cid: selectedCid})
  let [amount, setAmount] = useState(50)
  let [note, setNote] = useState('')

  function fetchStats() {
    if (o.cid && window.ipfs) {
      window.ipfs.object.stat(o.cid, (err, stats) => {
        if (stats) setObject(o => ({...o, stats}))
        if (err) setObject(o => ({...o, error: err.message}))
      })
    }
  }

  useEffect(
    () => {
      setObject({cid: selectedCid})
      setInvoice(null)
    },
    [selectedCid]
  )

  useEffect(fetchStats, [o.cid])

  let [invoice, setInvoice] = useState(null)
  let [orderId, setOrderId] = useState(null)

  if (invoice) {
    return (
      <Invoice
        orderId={orderId}
        cid={o.cid}
        amount={amount}
        note={note}
        invoice={invoice}
        setInvoice={setInvoice}
        onAfterPaid={() => {
          setInvoice(null)
          setOrderId(null)
          setObject({})
          setAmount(77)
          setNote('')

          onAfterPaid()
        }}
      />
    )
  }

  return (
    <form
      id="pin"
      onSubmit={async e => {
        e.preventDefault()
        let {invoice, order_id} = await createOrder({cid: o.cid, amount, note})
        if (invoice) {
          setOrderId(order_id)
          setInvoice(invoice)
        }
      }}
    >
      <label>
        <span
          data-balloon-length="small"
          data-balloon-pos="left"
          data-balloon="The IPFS CID, with or without the leading /ipfs/"
        >
          IPFS hash:
        </span>{' '}
        <input
          value={o.cid}
          onChange={e => setObject({cid: e.target.value})}
          onBlur={fetchStats}
        />
        {o.error ? (
          <div className="error">{o.error}</div>
        ) : (
          o.stats && (
            <div>
              <div>block size: {prettyBytes(o.stats.BlockSize)}</div>
              <div>number of links: {o.stats.NumLinks}</div>
              <div>cumulative size: {prettyBytes(o.stats.CumulativeSize)}</div>
            </div>
          )
        )}
      </label>
      <label>
        <span
          data-balloon-length="small"
          data-balloon-pos="left"
          data-balloon="You decide how much you want to pay now, we'll only know how much time does it buy for the given object afterwards."
        >
          Satoshis to pay:
        </span>{' '}
        <input
          type="number"
          min="1"
          step="1"
          value={amount}
          onChange={e => setAmount(e.target.value)}
        />
        {o.stats && (
          <div>time: {timeBought(amount, o.stats.CumulativeSize, priceGB)}</div>
        )}
      </label>
      <label>
        <span
          data-balloon-length="small"
          data-balloon-pos="left"
          data-balloon="You can use 4 characters for each satoshi you pay, this is meant to identify the object for visitors or yourself in the future. Optional."
        >
          Note to identify the object:
        </span>{' '}
        <input
          value={note}
          maxLength={amount * 4}
          onChange={e => setNote(e.target.value)}
        />
      </label>
      <button disabled={!o.cid || o.error || !amount}>Pin</button>
    </form>
  )
}

function Invoice({
  cid,
  amount,
  note,
  orderId,
  invoice,
  setInvoice,
  onAfterPaid
}) {
  let [paid, setPaid] = useState(false)
  var canceled = false

  async function waitPaid() {
    let order = await fetchOrder(orderId)
    if (order) {
      return true
    }
    if (canceled) {
      return false
    }
    return new Promise(resolve =>
      setTimeout(async () => {
        resolve(await waitPaid())
      }, 3000)
    )
  }

  useEffect(
    () => {
      waitPaid().then(v => {
        if (v) setPaid(true)
      })

      return () => {
        canceled = true
      }
    },
    [invoice, orderId]
  )

  useEffect(
    () => {
      if (paid) {
        setTimeout(onAfterPaid, 20000)
      }
    },
    [paid]
  )

  return (
    <div id="invoice">
      {paid ? (
        <>
          <h1 className="paid">PAID</h1>
          <p>You can close this page now.</p>
          <p>
            We'll try to fetch from the IPFS network for a while and as soon as
            we're successful it will show up in this page.
          </p>
          <p>If it fails a lot we'll give up.</p>
        </>
      ) : (
        <>
          <h2>Pay this invoice to pin your object</h2>
          <p>IPFS object: {cid}</p>
          <p>Value: {amount} satoshis</p>
          <p>Note: {note}</p>
          <QRCode
            bgColor="#FFFFFF"
            fgColor="#000000"
            level="Q"
            style={{width: '400px'}}
            value={invoice}
          />
          <p>{invoice}</p>
          <button
            onClick={e => {
              e.preventDefault()
              setInvoice(null)
            }}
          >
            Cancel
          </button>
        </>
      )}
    </div>
  )
}

async function createOrder({cid, note, amount}) {
  try {
    let res = await fetch('/api/order', {
      method: 'POST',
      body: JSON.stringify({cid, note, amount}),
      headers: {'Content-Type': 'application/json'}
    })
    if (!res.ok) throw new Error(await res.text())

    let {invoice, order_id} = await res.json()

    // save order id so this user will be able to edit his order
    // later if it is given up.
    localStorage.setItem(order_id, 1)

    return {invoice, order_id}
  } catch (err) {
    console.error(err)
    toast('failed to create order: ' + err.message, {
      type: 'error'
    })
  }
}

async function fetchOrder(orderId) {
  try {
    let res = await fetch(`/api/order/${orderId}`, {
      method: 'GET'
    })
    if (!res.ok) throw new Error(await res.text())
    return res.json()
  } catch (err) {
    console.error(err)
    toast('failed to fetch order: ' + err.message, {
      type: 'error'
    })
  }
}

function timeBought(amount, sizebytes, price_gb) {
  let days = amount / (sizebytes / 1000000000) / price_gb
  if (days < 1) {
    return 'nothing'
  }

  let now = new Date()
  let then = new Date().setDate(now.getDate() + days)

  return fromNow(then, {max: 2})
}
