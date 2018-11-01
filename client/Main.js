/** @format */

const fetch = window.fetch

import {ToastContainer, toast} from 'react-toastify'
import React, {useState, useEffect} from 'react' // eslint-disable-line no-unused-vars

export default function Main() {
  let [objects, setObjects] = useState([])
  let [selected, select] = useState(null)

  useEffect(async () => {
    setObjects(await fetchObjects())
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
          JSON.stringify(o)
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

  return (
    <form
      id="pin"
      onSubmit={async e => {
        e.preventDefault()

        let invoice = await createOrder({cid, amount, note})
        toast(invoice)
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
