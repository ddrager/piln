/** @format */

const fetch = window.fetch

import {ToastContainer, toast} from 'react-toastify'
import React, {useState, useEffect, useRef} from 'react' // eslint-disable-line no-unused-vars

import AddPin from './AddPin'
import PinnedObject from './PinnedObject'

export default function Main() {
  let [objects, setObjects] = useState([])
  let [selectedCid, select] = useState(undefined)

  async function loadObjects() {
    let objects = await fetchObjects()
    if (objects) setObjects(objects)
  }

  useEffect(loadObjects, [])

  return (
    <>
      <ToastContainer />
      <AddPin cid={selectedCid} onAfterPaid={loadObjects} />
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
