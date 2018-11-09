/** @format */

const fetch = window.fetch

import {ToastContainer, toast} from 'react-toastify'
import React, {useState, useEffect, useRef} from 'react' // eslint-disable-line no-unused-vars

import Portal from './Portal'
import PinnedObject from './PinnedObject'
import AddPin from './AddPin'

export const GlobalContext = React.createContext()

export default function Main() {
  let [{priceGB}, setGlobals] = useState({priceGB: 1000000000})
  let [objects, setObjects] = useState([])
  let [selectedCid, select] = useState(undefined)

  async function loadGlobals() {
    setGlobals(await fetchGlobals())
  }

  async function loadObjects() {
    let objects = await fetchObjects()
    if (objects) setObjects(objects)
  }

  useEffect(loadGlobals, [])
  useEffect(loadObjects, [])

  return (
    <>
      <GlobalContext.Provider value={priceGB}>
        <ToastContainer />
        <AddPin cid={selectedCid} onAfterPaid={loadObjects} />
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
  }
}
