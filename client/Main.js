/** @format */

const fetch = window.fetch

import {ToastContainer, toast} from 'react-toastify'
import Modal from 'react-modal'
import {QRCode} from 'react-qr-svg'
import React, {useState, useEffect, useRef} from 'react' // eslint-disable-line no-unused-vars

Modal.setAppElement(document.body)

export default function Main() {
  let [objects, setObjects] = useState([])
  let [selected, select] = useState(null)

  useEffect(async () => {
    let objects = await fetchObjects()
    if (objects) setObjects(objects)
  }, [])

  return (
    <>
      <ToastContainer />
      <AddPin cid={selected} />
      {objects.map(o => (
        <div
          onClick={() => {
            select(o)
          }}
        >
          {JSON.stringify(o)}
        </div>
      ))}
    </>
  )
}

function AddPin({object}) {
  object = object || {cid: ''}

  let [cid, cidUpdate] = useState(object.cid)
  let [amount, amountUpdate] = useState(77)
  let [note, noteUpdate] = useState('')

  let [invoice, invoiceUpdate] = useState(null)

  return (
    <>
      <Modal
        isOpen={!!invoice}
        onRequestClose={() => invoiceUpdate(null)}
        shouldCloseOnOverlayClick={true}
        contentLabel="Your invoice"
      >
        {invoice ? (
          <>
            <h1>Your invoice</h1>
            <p>
              IPFS object: {cid}
              <br />
              Value: {amount} satoshis
              <br />
              Note: {note}
            </p>
            <QRCode
              bgColor="#FFFFFF"
              fgColor="#000000"
              level="Q"
              style={{width: '400px'}}
              value={invoice}
            />
            <p>{invoice}</p>
          </>
        ) : null}
      </Modal>

      <form
        id="pin"
        onSubmit={async e => {
          e.preventDefault()

          let invoice = await createOrder({cid, amount, note})
          if (invoice) invoiceUpdate(invoice)
        }}
      >
        <label>
          <span>IPFS identifier:</span>{' '}
          <input value={cid} onChange={setFromChange(cidUpdate)} />
        </label>
        <label>
          <span>Satoshis to pay:</span>{' '}
          <input value={amount} onChange={setFromChange(amountUpdate)} />
        </label>
        <label>
          <span>Note to identify the object:</span>{' '}
          <input value={note} onChange={setFromChange(noteUpdate)} />
        </label>
        <button>Pin</button>
      </form>
    </>
  )
}

function setFromChange(fn) {
  return e => {
    fn(e.target.value)
  }
}

async function createOrder({cid, note, amount}) {
  try {
    let res = await fetch('/api/order', {
      method: 'POST',
      body: JSON.stringify({cid, note, amount}),
      headers: {'Content-Type': 'application/json'}
    })
    if (!res.ok) throw new Error(await res.text())
    return res.json()
  } catch (err) {
    console.error(err)
    toast('failed to create order: ' + err.message, {
      type: 'error'
    })
  }
}

async function fetchObjects() {
  try {
    let res = await fetch('/api/objects')
    if (!res.ok) throw new Error(await res.text())
    return res.json()
  } catch (err) {
    console.error(err)
    toast('failed to fetch objects: ' + err.message, {
      type: 'error'
    })
  }
}
