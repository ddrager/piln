/** @format */

const fetch = window.fetch

import {ToastContainer, toast} from 'react-toastify'
import React, {useState, useEffect, useRef} from 'react' // eslint-disable-line no-unused-vars

import Portal from './Portal'
import PinnedObject from './PinnedObject'
import AddPin from './AddPin'
import PaidWaiting from './PaidWaiting'
import orderStore from './orderStore'

export const GlobalContext = React.createContext()

export default function Main() {
  let [{priceGB, ipfsID, ipfsAddresses}, setGlobals] = useState({
    priceGB: 1000000000,
    ipfsID: 'temporarily offline'
  })
  let [objects, setObjects] = useState([])
  let [paidWaiting, setPaidWaiting] = useState([])
  let [selectedCid, select] = useState(undefined)

  async function loadObjects() {
    let objects = await fetchObjects()
    if (objects) setObjects(objects)
    setPaidWaiting(orderStore.list())
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

  useEffect(async () => {
    try {
      setGlobals(await fetchGlobals())
    } catch (err) {
      console.warn('IPFS node temporarily offline.')
    }
  }, [])
  useEffect(loadObjects, [])

  return (
    <>
      <GlobalContext.Provider value={{priceGB, ipfsID, ipfsAddresses}}>
        <ToastContainer />
        <AddPin
          cid={selectedCid}
          onAfterPaid={() => {
            loadObjects()
            connectRemote()
          }}
        />
        <Portal to="#price">{priceGB}</Portal>
        <div id="objects">
          {paidWaiting.map(orderId => (
            <PaidWaiting
              key={orderId}
              orderId={orderId}
              onProcessed={() => {
                loadObjects()
              }}
            />
          ))}
          {objects.map(o => (
            <PinnedObject
              {...o}
              key={o.cid}
              onSelect={e => {
                e.preventDefault()
                select(o.cid)
              }}
            />
          ))}
        </div>
        <Portal to="footer">
          <center>
            <p>Our IPFS node: {ipfsID}</p>
          </center>
        </Portal>
      </GlobalContext.Provider>
    </>
  )
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

async function fetchGlobals() {
  try {
    let res = await fetch('/api/globals')
    if (!res.ok) throw new Error(await res.text())
    return res.json()
  } catch (err) {
    console.error(err)
    toast('failed to fetch globals: ' + err.message, {
      type: 'error'
    })
    throw err
  }
}
