/** @format */

const fetch = window.fetch

import {ToastContainer, toast} from 'react-toastify'
import React, {useState, useEffect, useRef} from 'react' // eslint-disable-line no-unused-vars

import Portal from './Portal'
import PinnedObject from './PinnedObject'
import AddPin from './AddPin'
import NotPinnedObject from './NotPinnedObject'
import orderStore from './orderStore'

export const GlobalContext = React.createContext()

export default function Main() {
  let [{priceGB, ipfsID, ipfsAddresses}, setGlobals] = useState({
    priceGB: 1000000000,
    ipfsID: 'temporarily offline'
  })
  let [reusing, setReused] = useState({})
  let [objects, setObjects] = useState([])
  let [paidWaiting, setNotPinnedObject] = useState([])
  let [selectedCid, select] = useState(undefined)

  async function loadObjects() {
    let objects = await fetchObjects()
    if (objects) setObjects(objects)
    setNotPinnedObject(orderStore.list())
  }

  useEffect(() => {
    fetchGlobals()
      .then(setGlobals)
      .catch(err => {
        console.warn('IPFS node temporarily offline.', err)
      })
  }, [])
  useEffect(loadObjects, [])

  return (
    <>
      <GlobalContext.Provider value={{priceGB, ipfsID, ipfsAddresses}}>
        <ToastContainer />
        <AddPin
          cid={selectedCid}
          reused={Object.keys(reusing)
            .map(id => reusing[id])
            .filter(x => x)}
          onRemoveReused={e => {
            e.preventDefault()
            setReused({...reusing, [e.target.dataset.id]: null})
          }}
          onAfterPaid={() => {
            loadObjects()
            setReused({})
          }}
        />
        <Portal to="#price">{priceGB}</Portal>
        <div id="objects">
          {paidWaiting.filter(orderId => !reusing[orderId]).map(orderId => (
            <NotPinnedObject
              key={orderId}
              orderId={orderId}
              onProcessed={() => {
                loadObjects()
              }}
              onReuseSelect={e => {
                e.preventDefault()
                setReused({
                  ...reusing,
                  [e.target.dataset.id]: {
                    id: e.target.dataset.id,
                    note: e.target.dataset.note,
                    amount: e.target.dataset.amount
                  }
                })
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
