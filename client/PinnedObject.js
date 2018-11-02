/** @format */

const prettyBytes = require('pretty-bytes')
const fromNow = require('fromnow')

import React from 'react' // eslint-disable-line no-unused-vars

export default function PinnedObject({
  cid,
  sizegb,
  pinned_at,
  ends_at,
  notes,
  onSelect
}) {
  return (
    <div className="object">
      <h3>{cid}</h3>
      <table>
        <tbody>
          <tr>
            <td>Size:</td>
            <td>{prettyBytes(sizegb * 1000000000)}</td>
          </tr>
          <tr>
            <td>Pinned at:</td>
            <td>{pinned_at.split('T')[0]}</td>
          </tr>
          <tr>
            <td>Ends in:</td>
            <td title={ends_at.split('T')[0]}>{fromNow(ends_at, {max: 2})}</td>
          </tr>
          <tr>
            <td>Notes</td>
            <td>
              <ul>
                {notes.map(n => (
                  <li key={n}>{n}</li>
                ))}
              </ul>
            </td>
          </tr>
        </tbody>
      </table>
      <button onClick={onSelect}>Extend lifespan</button>
    </div>
  )
}
