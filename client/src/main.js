import { Elm } from './Main.elm'
import localforage from 'localforage'

const store = localforage.createInstance({
  name: '__dcrailz__',
  driver: localforage.INDEXEDDB
})

const seeds = window.crypto.getRandomValues(new Uint32Array(4))

const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: { seeds, now: Date.now() }
})

function listen () {
  let timeout
  const ws = new window.WebSocket(`ws://${window.location.host}`)
  ws.onmessage = function (ev) {
    if (!ev.data) return
    if (timeout) window.clearTimeout(timeout)
    timeout = window.setTimeout(function () {
      ws.close()
      setTimeout(listen, 0)
    }, 8 * 1000)

    app.ports.receivePredictions.send(JSON.parse(ev.data))
  }

  ws.onerror = function () { ws.close() }

  ws.onclose = function () { setTimeout(listen, 5000) }
}

listen()

store.getItem('location', function (err, value) {
  if (err !== null) {
    app.ports.receivedLocation.send({
      type: 'Failure',
      error: err instanceof Error ? err.message : 'Unknown error getting user location from cache'
    })
    return
  }
  const result = value
    ? { type: 'Loading' }
    : { type: 'NotAsked' }

  app.ports.receivedLocation.send(result)

  if (result.type === 'Loading') getLocation()
})

window.onblur = function () { app.ports.blurs.send({}) }

function locationSuccess (pos) {
  const location = [pos.coords.longitude, pos.coords.latitude]

  store.setItem('location', JSON.stringify(location))
  app.ports.receivedLocation.send({
    type: 'Success',
    value: location
  })
}

function locationError (err) {
  setTimeout(function () {
    app.ports.receivedLocation.send({
      type: 'Failure',
      error: err instanceof Error ? err.message : 'Unknown error getting user location'
    })
  }, 1500)
}

app.ports.askPosition.subscribe(getLocation)

function getLocation () {
  navigator.geolocation.getCurrentPosition(
    locationSuccess,
    locationError,
    { enableHighAccuracy: true }
  )
}
