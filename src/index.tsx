import 'bootstrap/dist/css/bootstrap.css'
import * as date from 'date-fns'
import * as _ from 'lodash'
import { h } from 'preact'
import * as preact from 'preact'

import './index.html'
import * as sat from './satellite-helper'

type LookAngle = sat.LookAngle & sat.Pass

const LOCATION: sat.Location = {
  latitude: 41.9,
  longitude: -71.42,
  elevation: 30,
}

interface AppProps {
  now: Date
  lookAngles: LookAngle[]
  passes: sat.Pass[]
}

const App = (props: AppProps) => (
  <div className="container" style={{ maxWidth: '980px', textAlign: 'center' }}>
    <LookAngleTable {...props} />
    <PassTable {...props} />
  </div>
)

interface LookAngleTableProps {
  now: Date
  lookAngles: LookAngle[]
}

const LookAngleTable = (props: LookAngleTableProps) => {
  const head = (
    <tr>
      <th>Satellite</th>
      <th>El</th>
      <th>Start → Apogee → End</th>
      <th>Az</th>
    </tr>
  )

  const rows = props.lookAngles.map(l => {
    const rising = props.now <= l.apogeeTime
    const arrow = rising ? '↑' : '↓'
    const rowClass = rising ? 'table-success' : 'table-primary'

    return (
      <tr className={rowClass}>
        <td>{l.satName}</td>
        <td>
          {arrow} {printDeg(l.el)} ({printDeg(l.apogeeEl)})
        </td>
        <td>
          {printTime(l.startTime)} → {printTime(l.apogeeTime)} →{' '}
          {printTime(l.endTime)}
        </td>
        <td>
          {printDeg(l.startAz)} → {printDeg(l.az)} → {printDeg(l.endAz)}
        </td>
      </tr>
    )
  })

  return (
    <table className="table">
      {head}
      {rows}
    </table>
  )
}

interface PassTableProps {
  now: Date
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

  const rows = props.passes
    .filter(p => p.startTime >= props.now)
    .map(p => <PassRow pass={p} />)

  return (
    <table className="table">
      {head}
      {rows}
    </table>
  )
}

interface PassRowProps {
  pass: sat.Pass
}

const PassRow = (props: PassRowProps) => {
  const p = props.pass

  let rowClass = ''
  if (p.apogeeEl >= 70) rowClass = 'table-danger'
  else if (p.apogeeEl >= 50) rowClass = 'table-warning'

  return (
    <tr className={rowClass}>
      <td>{date.format(p.startTime, 'iii M/dd')}</td>
      <td>{p.satName}</td>
      <td>{printDeg(p.apogeeEl)}</td>
      <td>
        {printTime(p.startTime)} → {printTime(p.apogeeTime)} →{' '}
        {printTime(p.endTime)}
      </td>
      <td>
        {printDeg(p.startAz)} → {printDeg(p.apogeeAz)} → {printDeg(p.endAz)}
      </td>
    </tr>
  )
}

function printTime(d: Date): string {
  return date.format(d, 'HH:mm')
}

function printDeg(deg: number): string {
  return `${Math.ceil(deg)}°`
}

function parseTLE(s: string): sat.TLE[] {
  const lines = s.split('\n').filter(line => line.length > 0)
  return _.chunk(lines, 3).map(l => ({
    satName: l[0],
    line1: l[1],
    line2: l[2],
  }))
}

function getPasses(tles: sat.TLE[], begin: Date, end: Date) {
  const passes = tles
    .map(tle => {
      try {
        return sat.getPasses(LOCATION, begin, end, tle)
      } catch (e) {
        console.debug(`Error calculating passes for ${tle.satName}: ${e}`)
        return []
      }
    })
    .flat()

  passes.sort(
    (p1: sat.Pass, p2: sat.Pass) =>
      p1.startTime.getTime() - p2.startTime.getTime()
  )

  return passes
}

function getLookAngles(
  tles: sat.TLE[],
  passes: sat.Pass[],
  now: Date
): LookAngle[] {
  passes = _.takeWhile(passes, p => p.startTime < now)
  passes = _.dropWhile(passes, p => p.endTime < now)
  return passes.map(pass => {
    const tle = tles.find(tle => tle.satName === pass.satName)
    if (!tle)
      throw new Error(
        `Invalid state: Pass exists for ${pass.satName} but no corresponding TLE`
      )
    const lookAngle = sat.getLookAngle(LOCATION, now, tle)
    return { ...lookAngle, ...pass }
  })
}

function renderApp(nasabare: string) {
  const tles = parseTLE(nasabare)
  let now = new Date(),
      begin = date.subMinutes(now, 15),
      end = date.addHours(now, 1),
      passes = getPasses(tles, begin, end)

  const update = () => {
    now = new Date()
    begin = end
    end = date.addHours(now, 1)

    const newPasses = getPasses(tles, begin, end)
    if (newPasses.length > 0) console.log('Loaded new passes:', newPasses)
    passes = _.dropWhile(passes, pass => pass.endTime < now)
      .concat(newPasses)

    const lookAngles = getLookAngles(tles, passes, now)
    preact.render(<App {...{ now, lookAngles, passes }} />, document.body)
  }

  update()
  setInterval(update, 5000)
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
