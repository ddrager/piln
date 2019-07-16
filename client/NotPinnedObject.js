/** @format */

const fetch = window.fetch

import React, {useEffect, useState} from 'react' // eslint-disable-line no-unused-vars
import {toast} from 'react-toastify'
import orderStore from './orderStore'

export default function NotPinnedObject({orderId, onProcessed, onReuseSelect}) {
  let [payment, setPayment] = useState({})
  let [i, setI] = useState(1)

	console.log("orderId, onProcessed, onReuseSelect",orderId,onProcessed,onReuseSelect)

  useEffect(
    () => {
	async function getPayment() {
      if (payment.status !== 'given_up') {
        setTimeout(async () => {
          setPayment(await fetchPayment(orderId))
          setI(i + 1)
        }, i * 5000)
      }

      // if the payment was pinned, delete the pending stuff
      if (
        payment &&
        payment.status &&
        (payment.status !== 'trying' && payment.status !== 'given_up')
      ) {
        orderStore.remove(orderId)
        onProcessed()
      }

      // if the payment is null means we haven't paid yet
      if (!payment && i > 2 /* give 15 seconds for the race */) {
        // if we aren't viewing the invoice in the screen we aren't trying to pay it
        // so delete this pending order id
        let invoice = document.querySelector('#invoice')
        if (!invoice || invoice.dataset.order !== orderId) {
          orderStore.remove(orderId)
          onProcessed()
        }
      }
    }
    getPayment();
    }, [i])

  if (!payment || !payment.order_id) {
	  console.log("no payment or no payment order id",payment)
    return null
  }

  let {order_id, cid, amount, note, paid_at, tries, status} = payment

  return (
    <div className={`object ${status}`}>
      {status === 'given_up' ? (
        <p>
          given up{' '}
          <button
            data-id={order_id}
            data-note={note}
            data-amount={amount}
            onClick={onReuseSelect}
            data-balloon-length="small"
            data-balloon-pos="right"
            data-balloon="You can repurpose this failed order in a new pin request."
          >
            Reuse paid amount
          </button>
        </p>
      ) : (
        <p>paid, pinning in progress</p>
      )}
      <table>
        <tbody>
          <tr>
            <td>Date: </td>
            <td>{paid_at}</td>
          </tr>
          <tr>
            <td>Satoshis: </td>
            <td>{amount}</td>
          </tr>
          <tr>
            <td>Note: </td>
            <td>{note}</td>
          </tr>
          <tr>
            <td>Hash: </td>
            <td>
              <a href={`https://ipfs.io/ipfs/${cid}`} target="_blank">
                {cid}
              </a>
            </td>
          </tr>
          <tr>
            <td>Attempt: </td>
            <td>{tries}</td>
          </tr>
        </tbody>
      </table>
    </div>
  )
}

async function fetchPayment(orderId) {
  try {
    let res = await fetch('/api/order/' + orderId)
    if (!res.ok) throw new Error(await res.text())
    return res.json()
  } catch (err) {
    console.error(err)
    toast('failed to fetch payment: ' + err.message, {
      type: 'error'
    })
  }
}
