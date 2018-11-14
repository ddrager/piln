/** @format */

const fetch = window.fetch

import React, {useEffect, useState} from 'react' // eslint-disable-line no-unused-vars
import {toast} from 'react-toastify'
import orderStore from './orderStore'

export default function PaidWaiting({orderId, onProcessed}) {
  let [payment, setPayment] = useState()
  let [i, setI] = useState(1)

  useEffect(
    async () => {
      setTimeout(async () => {
        let payment = await fetchPayment(orderId)
        setI(i + 1)
        setPayment(payment)
      }, i * 5000)

      // if the payment is processed, delete the pending stuff
      if (payment && payment.processed && !payment.given_up) {
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
    },
    [i]
  )

  if (!payment) {
    return null
  }

  let {cid, amount, note, paid_at, tries, given_up} = payment

  return (
    <div className="object paid-waiting">
      {given_up ? <p>given up</p> : <p>paid, not pinned yet</p>}
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
