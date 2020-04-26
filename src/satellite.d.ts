declare module 'satellite.js' {
  export type GMST = { readonly _tag: 'GMST' }

  export type ECI = { readonly _tag: 'ECI' }

  export type ECF = { x: number; y: number; z: number }

  export type ObserverGd = {
    latitude: number
    longitude: number
    height: number
  }

  export type SatRec = { error: number }

  export type PositionAndVelocityECI = {
    position: ECI
    velocity: ECI
  }

  export type PositionAndVelocityECF = {
    position: ECF
    velocity: ECF
  }

  export type LookAngle = {
    elevation: number
    azimuth: number
  }

  export function twoline2satrec(line1: string, line2: string): SatRec

  export function propagate(satRec: SatRec, date: Date): PositionAndVelocityECI

  export function geodeticToEcf(observerGd: ObserverGd): ECF

  export function eciToEcf(eci: ECI, gmst: GMST): ECF

  export function gstime(date: Date): GMST

  export function ecfToLookAngles(
    observerGd: ObserverGd,
    position: ECF,
    velocity: ECF
  ): LookAngle
}
