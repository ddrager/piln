/** @format */

const fetch = window.fetch

import React, {useState, useEffect} from 'react' // eslint-disable-line no-unused-vars
import {QRCode} from 'react-qr-svg'
import {toast} from 'react-toastify'

export default function AddPin({cid: selectedCid = '', onAfterPaid}) {
  let [cid, cidUpdate] = useState(selectedCid)
  let [amount, amountUpdate] = useState(77)
  let [note, noteUpdate] = useState('')

  useEffect(
    () => {
      cidUpdate(selectedCid)
      invoiceUpdate(null)
    },
    [selectedCid]
  )

  const setFromChange = fn => e => fn(e.target.value)

  let [invoice, invoiceUpdate] = useState(null)
  let [orderId, orderIdUpdate] = useState(null)

  if (invoice) {
    return (
      <Invoice
        orderId={orderId}
        cid={cid}
        amount={amount}
        note={note}
        invoice={invoice}
        invoiceUpdate={invoiceUpdate}
        onAfterPaid={() => {
          invoiceUpdate(null)
          orderIdUpdate(null)
          cidUpdate('')
          amountUpdate(77)
          noteUpdate('')

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

        let {invoice, order_id} = await createOrder({cid, amount, note})
        if (invoice) {
          orderIdUpdate(order_id)
          invoiceUpdate(invoice)
        }
      }}
    >
      <label>
        <span
          data-balloon-length="small"
          data-balloon-pos="left"
          data-balloon="The IPFS CID, with or without the leading /ipfs/"
        >
          IPFS identifier:
        </span>{' '}
        <input value={cid} onChange={setFromChange(cidUpdate)} />
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
          onChange={setFromChange(amountUpdate)}
        />
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
          onChange={setFromChange(noteUpdate)}
        />
      </label>
      <button>Pin</button>
    </form>
  )
}

function Invoice({
  cid,
  amount,
  note,
  orderId,
  invoice,
  invoiceUpdate,
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
              invoiceUpdate(null)
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
