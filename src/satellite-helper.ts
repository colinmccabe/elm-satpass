import { ObserverGd, PositionAndVelocityECF, SatRec } from 'satellite.js'
import * as satellite from 'satellite.js'

export interface Location {
  latitude: number
  longitude: number
  elevation: number
}

export interface TLE {
  satName: string
  line1: string
  line2: string
}

export interface Pass {
  satName: string
  startTime: Date
  startAz: number
  apogeeTime: Date
  apogeeEl: number
  apogeeAz: number
  endTime: Date
  endAz: number
}

function toDeg(rad: number) {
  return rad * 57.2957795
}

function toRad(deg: number) {
  return deg * 0.0174532925
}

function observerGd(loc: Location) {
  return {
    latitude: toRad(loc.latitude),
    longitude: toRad(loc.longitude),
    height: loc.elevation / 1000,
  }
}

function calcPositionAndVelocityEcf(
  satrec: SatRec,
  date: Date
): PositionAndVelocityECF {
  const gmst = satellite.gstime(date)

  const pvEci = satellite.propagate(satrec, date)

  if (satrec.error !== 0)
    throw Error('error #' + satrec.error + ' calculating position and velocity')

  return {
    position: satellite.eciToEcf(pvEci.position, gmst),
    velocity: satellite.eciToEcf(pvEci.velocity, gmst),
  }
}

// function calcDopplerFactor(observerPos, satEcf) {
//   const satPos = satEcf.position
//   const satVelocity = satEcf.velocity

//   const currentRange = Math.sqrt(
//     Math.pow(satPos.x - observerPos.x, 2) +
//       Math.pow(satPos.y - observerPos.y, 2) +
//       Math.pow(satPos.z - observerPos.z, 2)
//   )

//   const nextSatPos = {
//     x: satPos.x + satVelocity.x,
//     y: satPos.y + satVelocity.y,
//     z: satPos.z + satVelocity.z,
//   }

//   const nextRange = Math.sqrt(
//     Math.pow(nextSatPos.x - observerPos.x, 2) +
//       Math.pow(nextSatPos.y - observerPos.y, 2) +
//       Math.pow(nextSatPos.z - observerPos.z, 2)
//   )

//   const rangeRate = currentRange - nextRange

//   const c = 299792.458 // Speed of light in km/s
//   const factor = 1 + rangeRate / c
//   return factor
// }

function calcLookAngle(satrec: SatRec, observerGd: ObserverGd, date: Date) {
  const pvEcf = calcPositionAndVelocityEcf(satrec, date)
  return satellite.ecfToLookAngles(observerGd, pvEcf.position, pvEcf.velocity)
}

function nextPass(
  observerGd: ObserverGd,
  satName: string,
  satrec: SatRec,
  searchBegin: Date
): Pass | null {
  const searchBeginMs = searchBegin.getTime()
  let date = new Date(searchBegin.getTime())
  let lookAngle = null

  // Step forward coarsely until el > 0
  do {
    lookAngle = calcLookAngle(satrec, observerGd, date)
    date = new Date(date.getTime() + 60000)
  } while (
    lookAngle.elevation < 0.0 &&
    date.getTime() - searchBeginMs < 86400000
  )

  if (lookAngle === null) {
    return null
  }

  // Step back finely to pass start
  while (lookAngle.elevation > 0.0) {
    date = new Date(date.getTime() - 1000)
    lookAngle = calcLookAngle(satrec, observerGd, date)
  }
  date = new Date(date.getTime() + 1000)
  lookAngle = calcLookAngle(satrec, observerGd, date)

  const startTime = date
  const startAz = lookAngle.azimuth

  // Step forward to pass end, record apogee
  let apogeeEl = 0.0
  let apogeeTime = date
  let apogeeAz = startAz

  while (lookAngle.elevation > 0.0) {
    lookAngle = calcLookAngle(satrec, observerGd, date)
    if (lookAngle.elevation > apogeeEl) {
      apogeeEl = lookAngle.elevation
      apogeeTime = date
      apogeeAz = apogeeEl
    }
    date = new Date(date.getTime() + 1000)
  }

  const endTime = date
  const endAz = lookAngle.azimuth

  return {
    satName: satName,
    startTime: startTime,
    startAz: toDeg(startAz),
    apogeeTime: apogeeTime,
    apogeeEl: toDeg(apogeeEl),
    apogeeAz: toDeg(apogeeAz),
    endTime: endTime,
    endAz: toDeg(endAz),
  }
}

export function getPasses(
  loc: Location,
  begin: Date,
  end: Date,
  tle: TLE
): Pass[] {
  const satrec = satellite.twoline2satrec(tle.line1, tle.line2)
  const gd = observerGd(loc)

  const passes = []

  while (begin < end) {
    const pass = nextPass(gd, tle.satName, satrec, begin)

    if (pass === null || pass.startTime > end) {
      break
    }

    passes.push(pass)
    begin = new Date(pass.endTime)
  }

  return passes.filter(pass => pass.apogeeEl > 5.0)
}

// export function getLookAngles(req) {
//   const lookAngles = []

//   req.sats.forEach(function (sat) {
//     const date = new Date(req.time)
//     const gd = observerGd(req.latitude, req.longitude, req.altitude)
//     const satrec = satellite.twoline2satrec(sat.tle.line1, sat.tle.line2)

//     const observerEcf = satellite.geodeticToEcf(gd)
//     const lookAngle = calcLookAngle(satrec, gd, date)
//     const satEcf = calcPositionAndVelocityEcf(satrec, date)
//     const dopplerFactor = calcDopplerFactor(observerEcf, satEcf)

//     lookAngles.push({
//       id: sat.id,
//       elevation: toDeg(lookAngle.elevation),
//       azimuth: toDeg(lookAngle.azimuth),
//       dopplerFactor: dopplerFactor,
//     })
//   })

//   return lookAngles
// }
