/** @format */

const prettyBytes = require('pretty-bytes')
const fromNow = require('fromnow')
const uniq = require('array-uniq')

import React, {useEffect, useState} from 'react' // eslint-disable-line no-unused-vars

export default function PinnedObject({
  cid,
  sizegb,
  pinned_at,
  ends_at,
  notes,
  onSelect
}) {
  let [provs, setProvs] = useState(null)
  let [showAllProvs, setShowAllProvs] = useState(false)

  useEffect(() => {
    if (window.ipfs) {
      window.ipfs.dht
        .findprovs(cid)
        .catch(err => console.warn('error finding provs for ' + cid, err))
        .then(peerInfos => {
          setProvs(uniq(peerInfos.map(p => p.ID).filter(x => x)))
        })
    }
  }, [])

  return (
    <div className="object">
      <h3>
        <a href={`https://ipfs.io/ipfs/${cid}`} target="_blank">
          {cid}
        </a>
      </h3>
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
          {provs && (
            <tr>
              <td>Providers:</td>
              <td>
                <ul>
                  {provs.slice(0, showAllProvs ? Infinity : 5).map(p => (
                    <li key={p}>
                      {p.slice(0, 7)}
                      ..
                      {p.slice(-7)}
                    </li>
                  ))}
                  {provs.length > 5 && (
                    <li>
                      <a
                        onClick={() => {
                          setShowAllProvs(!showAllProvs)
                        }}
                      >
                        {showAllProvs
                          ? 'hide'
                          : `show more (${provs.length} total)`}
                      </a>
                    </li>
                  )}
                </ul>
              </td>
            </tr>
          )}
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
