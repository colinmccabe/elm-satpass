import './index.html'
import 'bootstrap/dist/css/bootstrap.css'
import * as date from 'date-fns'
import * as _ from 'lodash'
import { h } from 'preact'
import * as preact from 'preact'

import * as sat from './satellite-helper'

const LOCATION: sat.Location = {
  latitude: 41.9,
  longitude: -71.42,
  elevation: 30,
}

interface AppProps {
  passes: sat.Pass[]
}

const App = (props: AppProps) => (
  <div className="container" style={{ maxWidth: '980px' }}>
    <PassTable passes={props.passes} />
  </div>
)

interface PassTableProps {
  passes: sat.Pass[]
}

const PassTable = (props: PassTableProps) => {
  const head = (
    <tr>
      <th>Date</th>
      <th>Satellite</th>
      <th>Max El</th>
      <th>Start → Apogee → End</th>
      <th>Az</th>
    </tr>
  )

  const rows = props.passes.map(p => <PassRow pass={p} now={new Date()} />)

  return (
    <table className="table" style={{ textAlign: 'center' }}>
      {head}
      {rows}
    </table>
  )
}

interface PassRowProps {
  pass: sat.Pass
  now: Date
}

const PassRow = (props: PassRowProps) => {
  const p = props.pass
  const now = props.now

  let rowClass = ''
  if (now > p.endTime) rowClass = 'text-muted table-active'
  else if (now > p.startTime && now < p.endTime) rowClass = 'table-info'
  else if (p.apogeeEl >= 70) rowClass = 'table-danger'
  else if (p.apogeeEl >= 50) rowClass = 'table-warning'

  return (
    <tr className={rowClass}>
      <td>{date.format(p.startTime, 'iii M/dd')}</td>
      <td>{p.satName}</td>
      <td>{Math.ceil(p.apogeeEl)}°</td>
      <td>
        {showTime(p.startTime)} → {showTime(p.apogeeTime)} →{' '}
        {showTime(p.endTime)}
      </td>
      <td>
        {Math.ceil(p.startAz)}° → {Math.ceil(p.apogeeAz)}° →{' '}
        {Math.ceil(p.endAz)}°
      </td>
    </tr>
  )
}

function showTime(d: Date): string {
  return date.format(d, 'HH:mm')
}

function parseTLE(s: string): sat.TLE[] {
  const lines = s.split('\n').filter(line => line.length > 0)
  return _.chunk(lines, 3).map(l => ({
    satName: l[0],
    line1: l[1],
    line2: l[2],
  }))
}

function renderApp(nasabare: string) {
  const tles = parseTLE(nasabare)

  const now = new Date()
  const begin = date.subMinutes(now, 15)
  const end = date.addHours(now, 1)
  const passes = tles
    .map(tle => sat.getPasses(LOCATION, begin, end, tle))
    .flat()
  passes.sort(
    (p1: sat.Pass, p2: sat.Pass) =>
      p1.startTime.getTime() - p2.startTime.getTime()
  )

  preact.render(<App passes={passes} />, document.body)
}

fetch('nasabare.txt')
  .then(resp => {
    if (!resp.ok) {
      throw new Error(`Got response code ${resp.status} for nasabare.txt`)
    }

    return resp.text()
  })
  .then(renderApp)
  .catch((e: Error) => {
    console.log(e)
    preact.render(<p>{e.message}</p>, document.body)
  })
