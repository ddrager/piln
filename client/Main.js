/** @format */

const fetch = window.fetch

import {ToastContainer, toast} from 'react-toastify'
import React, {useState, useEffect} from 'react' // eslint-disable-line no-unused-vars

export default function Main() {
  let [objects, setObjects] = useState([])

  useEffect(() => {
    fetchObjects()
      .then(setObjects)
      .catch(toastError)
  }, [])

  return (
    <>
      <ToastContainer />
      {objects.map(o => (
        <div>JSON.stringify(o)</div>
      ))}
    </>
  )
}

function toastError(err) {
  toast(err.message, {
    type: 'error'
  })
}

async function fetchObjects() {
  try {
    let res = await fetch('/api/objects')
    let r = await res.json()
    return r
  } catch (err) {
    console.error(err)
    throw new Error('failed to fetch objects')
  }
}
