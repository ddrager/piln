/** @format */

const fetch = window.fetch

import {ToastContainer, toast} from 'react-toastify'
import React, {useState, useEffect, useRef} from 'react' // eslint-disable-line no-unused-vars

import Portal from './Portal'
import PinnedObject from './PinnedObject'
import AddPin from './AddPin'

export const GlobalContext = React.createContext()

export default function Main() {
  let [offline, setOffline] = useState(false)
  let [{priceGB, ipfsID, ipfsAddresses}, setGlobals] = useState({
    priceGB: 1000000000
  })
  let [objects, setObjects] = useState([])
  let [selectedCid, select] = useState(undefined)

  async function loadObjects() {
    let objects = await fetchObjects()
    if (objects) setObjects(objects)
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
      setOffline(true)
    }
  }, [])
  useEffect(loadObjects, [])

  if (offline) {
    return (
      <h1>
        Our IPFS node is offline, so this service is not operational right now.
        <p>
          Please contact us at piln@alhur.es if you think we don't know that
          already and must be alerted by email to solve the issue, which is
          probably the reality of the situation.
        </p>
      </h1>
    )
  }

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
        <center>
          <p>Our IPFS node: {ipfsID}</p>
        </center>
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
