/** @format */

const fetch = window.fetch
const fromNow = require('fromnow')
const prettyBytes = require('prettier-bytes')

import React, {useState, useEffect, useContext} from 'react' // eslint-disable-line no-unused-vars
import {QRCode} from 'react-qr-svg'
import {toast} from 'react-toastify'

import {GlobalContext} from './Main'
import orderStore from './orderStore'

export default function AddPin({
  cid: selectedCid = '',
  reused = [],
  onRemoveReused,
  onAfterPaid
}) {
  let {priceGB, ipfsAddresses} = useContext(GlobalContext)

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

  function connectRemote() {
    if (ipfsAddresses && window.ipfs) {
      ipfsAddresses.forEach(addr => {
        window.ipfs.swarm.connect(
          addr,
          console.log
        )
      })
    }
  }

  function afterPaid() {
    setInvoice(null)
    setOrderId(null)
    setObject({})
    setAmount(77)
    setNote('')
    connectRemote()
    onAfterPaid()
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
        nreused={reused.length}
        note={note}
        invoice={invoice}
        setInvoice={setInvoice}
        onAfterPaid={afterPaid}
      />
    )
  }

  return (
    <form
      id="pin"
      onSubmit={async e => {
        e.preventDefault()
        let {invoice, order_id} = await createOrder({
          cid: o.cid,
          amount,
          note,
          reused_orders: reused.map(({id}) => id)
        })

        if (order_id) {
          // success.
          setOrderId(order_id)
          if (invoice) {
            // show invoice.
            setInvoice(invoice)
          } else {
            // there's no invoice, it's already paid.
            afterPaid()
          }
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
          value={o.cid || ''}
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
          data-balloon="You decide how much you want to pay now, we'll only know how much time does it buy for the given object afterwards (unless you're using window,ipfs, in that case we can give you an approximation)."
        >
          Satoshis to pay:
        </span>{' '}
        <input
          type="number"
          step="1"
          min="0"
          value={amount}
          onChange={e => setAmount(e.target.value)}
        />
        {o.stats && (
          <div>time: {timeBought(amount, o.stats.CumulativeSize, priceGB)}</div>
        )}
      </label>
      {reused.length === 0 ? null : (
        <label
          data-balloon-length="small"
          data-balloon-pos="left"
          data-balloon="The amount paid for this order will be reused in the next."
        >
          Reusing orders:
          <div className="reusing">
            {reused.map(({id, note, amount}) => (
              <div key={id}>
                <div className="note">{note}</div>
                <div className="amount">{amount}</div>
                <button data-id={id} onClick={onRemoveReused}>
                  ×
                </button>
              </div>
            ))}
          </div>
        </label>
      )}
      <label>
        <span
          data-balloon-length="small"
          data-balloon-pos="left"
          data-balloon="Optional. Meant to identify the object for visitors or yourself in the future. You can use up to 23 characters or 1 character for each satoshi you pay, which one is bigger."
        >
          Note to identify the object:
        </span>{' '}
        <input
          value={note}
          maxLength={amount < 23 ? 23 : amount}
          onChange={e => setNote(e.target.value)}
        />
      </label>
      <button
        disabled={
          !o.cid || o.error || (parseInt(amount) === 0 && reused.length === 0)
        }
      >
        Pin
      </button>
    </form>
  )
}

function Invoice({
  cid,
  amount,
  nreused,
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
        setTimeout(onAfterPaid, 10000)
      }
    },
    [paid]
  )

  return (
    <div id="invoice" data-order={orderId}>
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
          <p>
            Value: {amount} satoshis
            {nreused > 0
              ? ` (plus ${nreused} reused order ${nreused === 1 ? '' : 's'})`
              : ''}
          </p>
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

async function createOrder({cid, note, amount, reused_orders}) {
  try {
    let res = await fetch('/api/order', {
      method: 'POST',
      body: JSON.stringify({cid, note, amount, reused_orders}),
      headers: {'Content-Type': 'application/json'}
    })
    if (!res.ok) throw new Error(await res.text())

    let {invoice, order_id} = await res.json()

    // save order id so this user will be able to see his order later
    orderStore.add(order_id)

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
